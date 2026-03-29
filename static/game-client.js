// Connect 4 — Game Client
// Token-based API with Redis-backed game slots

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

let gameActive = false;
let isProcessing = false;
let selectedColumn = 3;
let previousBoard = null;
let gameToken = null;

// ---------------------------------------------------------------------------
// DOM references
// ---------------------------------------------------------------------------

const statusEl = document.getElementById('status');
const statusTextEl = document.getElementById('status-text');
const spinnerEl = document.getElementById('spinner');
const statsEl = document.getElementById('stats');
const depthSelect = document.getElementById('depth');
const newGameBtn = document.getElementById('new-game');
const settingsRow = document.getElementById('settings-row');
const slotsEl = document.getElementById('slot-count');
const slotsRefreshBtn = document.getElementById('slot-refresh');
const cellElements = [];

// ---------------------------------------------------------------------------
// Slot availability
// ---------------------------------------------------------------------------

async function refreshSlotCount() {
    try {
        slotsRefreshBtn.classList.add('refreshing');
        const res = await fetch('/api/health');
        const data = await res.json();
        const active = data.activeGames ?? data['active-games'] ?? 0;
        const max = data.maxGames ?? data['max-games'] ?? 4;
        slotsEl.textContent = `${active}/${max} slots`;
        slotsEl.classList.toggle('slots-full', active >= max);
    } catch {
        slotsEl.textContent = '--/-- slots';
    } finally {
        slotsRefreshBtn.classList.remove('refreshing');
    }
}

slotsRefreshBtn.addEventListener('click', refreshSlotCount);

// ---------------------------------------------------------------------------
// Status helpers
// ---------------------------------------------------------------------------

function setStatus(text, showSpinner = false) {
    statusTextEl.textContent = text;
    spinnerEl.classList.toggle('active', showSpinner);
}

// ---------------------------------------------------------------------------
// Board rendering
// ---------------------------------------------------------------------------

function initBoard() {
    const board = document.getElementById('board');
    board.innerHTML = '';
    cellElements.length = 0;
    for (let row = 5; row >= 0; row--) {
        for (let col = 0; col < 7; col++) {
            if (!cellElements[col]) cellElements[col] = [];
            const cell = document.createElement('div');
            cell.className = 'cell';
            cell.addEventListener('click', () => makeMove(col));
            board.appendChild(cell);
            cellElements[col][row] = cell;
        }
    }
    document.querySelectorAll('.col-num').forEach(el => {
        el.addEventListener('click', () => makeMove(parseInt(el.dataset.col)));
    });
    document.addEventListener('keydown', handleKeydown);
}

function clearBoard() {
    for (let col = 0; col < 7; col++) {
        for (let row = 0; row < 6; row++) {
            if (cellElements[col]?.[row]) cellElements[col][row].className = 'cell';
        }
    }
    previousBoard = null;
}

function updateBoard(board) {
    if (!board) return;
    for (let col = 0; col < 7; col++) {
        for (let row = 0; row < 6; row++) {
            const cell = cellElements[col][row];
            const value = board[col][row];
            const prevValue = previousBoard?.[col]?.[row];
            if (value === 'X') {
                if (prevValue !== 'X') {
                    const dropDistance = -((6 - row) * 58);
                    cell.style.setProperty('--drop-distance', `${dropDistance}px`);
                    cell.className = 'cell player dropping';
                    setTimeout(() => cell.classList.remove('dropping'), 400);
                } else {
                    cell.className = 'cell player';
                }
            } else if (value === 'O') {
                if (prevValue !== 'O') {
                    const dropDistance = -((6 - row) * 58);
                    cell.style.setProperty('--drop-distance', `${dropDistance}px`);
                    cell.className = 'cell ai dropping';
                    setTimeout(() => cell.classList.remove('dropping'), 400);
                } else {
                    cell.className = 'cell ai';
                }
            } else {
                cell.className = 'cell';
            }
        }
    }
    previousBoard = JSON.parse(JSON.stringify(board));
}

function highlightWinningCells(cells) {
    if (!cells?.length) return;
    setTimeout(() => {
        cells.forEach(([col, row]) => cellElements[col]?.[row]?.classList.add('winning'));
    }, 300);
}

// ---------------------------------------------------------------------------
// Column selection (keyboard navigation)
// ---------------------------------------------------------------------------

function updateColumnSelection() {
    document.querySelectorAll('.col-num').forEach((el, i) => {
        el.classList.toggle('selected', i === selectedColumn && gameActive);
    });
}

function handleKeydown(e) {
    if (e.key >= '1' && e.key <= '7') {
        if (gameActive) makeMove(parseInt(e.key) - 1);
        return;
    }
    switch (e.key) {
        case 'ArrowLeft':
            e.preventDefault();
            selectedColumn = Math.max(0, selectedColumn - 1);
            updateColumnSelection();
            break;
        case 'ArrowRight':
            e.preventDefault();
            selectedColumn = Math.min(6, selectedColumn + 1);
            updateColumnSelection();
            break;
        case 'Enter': case ' ':
            e.preventDefault();
            if (gameActive) makeMove(selectedColumn);
            break;
        case 'n': case 'N':
            if (!e.ctrlKey && !e.metaKey)
                newGame(document.getElementById('ai-first-toggle').checked);
            break;
    }
}

// ---------------------------------------------------------------------------
// Game lifecycle
// ---------------------------------------------------------------------------

async function newGame(aiFirst = false) {
    clearBoard();
    isProcessing = false;
    try {
        const res = await fetch('/api/new-game', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                depth: parseInt(depthSelect.value),
                aiFirst: aiFirst
            })
        });

        if (res.status === 503) {
            const data = await res.json();
            setStatus(data.message || 'All game slots in use — try again in a moment');
            refreshSlotCount();
            return;
        }

        const data = await res.json();
        if (data.error) { setStatus(data.message || data.error); return; }

        gameToken = data.token;
        sessionStorage.setItem('gameToken', gameToken);
        gameActive = true;
        selectedColumn = 3;
        newGameBtn.textContent = 'Resign';
        newGameBtn.classList.add('game-active');
        settingsRow.classList.add('locked');
        updateBoard(data.board);
        updateColumnSelection();
        setStatus(data.message);
        statusEl.className = 'status';
        statsEl.textContent = '';
        clearDebug();
        refreshSlotCount();
        if (aiFirst && data) showDebug(data);
    } catch {
        setStatus('Connection error');
    }
}

function endGame() {
    gameActive = false;
    gameToken = null;
    sessionStorage.removeItem('gameToken');
    newGameBtn.textContent = 'New Game';
    newGameBtn.classList.remove('game-active');
    settingsRow.classList.remove('locked');
    spinnerEl.classList.remove('active');
    refreshSlotCount();
}

function findNextEmptyRow(col) {
    for (let row = 0; row < 6; row++) {
        const cell = cellElements[col][row];
        if (!cell.classList.contains('player') && !cell.classList.contains('ai')) return row;
    }
    return null;
}

// ---------------------------------------------------------------------------
// Make a move
// ---------------------------------------------------------------------------

async function makeMove(column) {
    if (!gameActive || !gameToken) { setStatus('Press New Game to start'); return; }
    if (isProcessing) return;

    if (cellElements[column][5].classList.contains('player') ||
        cellElements[column][5].classList.contains('ai')) return;

    isProcessing = true;
    const playerRow = findNextEmptyRow(column);

    // Optimistic UI — drop player piece immediately
    if (playerRow !== null) {
        const dropDistance = -((6 - playerRow) * 58);
        cellElements[column][playerRow].style.setProperty('--drop-distance', `${dropDistance}px`);
        cellElements[column][playerRow].className = 'cell player dropping';
        setTimeout(() => cellElements[column][playerRow].classList.remove('dropping'), 400);
        if (!previousBoard) {
            previousBoard = Array(7).fill(null).map(() => Array(6).fill(null));
        }
        previousBoard[column][playerRow] = 'X';
    }

    setStatus('AI thinking...', true);
    const thinkingStartTime = Date.now();

    try {
        const res = await fetch('/api/move', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ token: gameToken, column: column })
        });

        // Handle expired/invalid game
        if (res.status === 404) {
            setStatus('Game expired. Start a new game.');
            gameToken = null;
            endGame(); // endGame already calls refreshSlotCount
            return;
        }

        const data = await res.json();

        if (data.error) {
            setStatus(data.message || data.error);
            // Roll back optimistic UI
            if (playerRow !== null) {
                cellElements[column][playerRow].className = 'cell';
                previousBoard[column][playerRow] = null;
            }
            isProcessing = false;
            return;
        }

        // Minimum thinking time for UX
        const elapsed = Date.now() - thinkingStartTime;
        if (elapsed < 1200) {
            await new Promise(r => setTimeout(r, 1200 - elapsed));
        }

        updateBoard(data.board);
        setStatus(data.message);
        if (data.evaluations) {
            statsEl.textContent = `${data.evaluations.toLocaleString()} positions`;
        }
        showDebug(data);

        const winningCells = data.winningCells || data['winning-cells'];
        if (data.status === 'player_wins') {
            setStatus('You win!');
            statusEl.className = 'status win';
            endGame();
            highlightWinningCells(winningCells);
        } else if (data.status === 'ai_wins') {
            setStatus('AI wins');
            statusEl.className = 'status lose';
            endGame();
            highlightWinningCells(winningCells);
        } else if (data.status === 'draw') {
            setStatus('Draw');
            endGame();
        } else {
            statusEl.className = 'status';
            refreshSlotCount();
        }
    } catch {
        setStatus('Connection error');
        if (playerRow !== null) cellElements[column][playerRow].className = 'cell';
    }
    isProcessing = false;
    updateColumnSelection();
}

// ---------------------------------------------------------------------------
// Resign handler
// ---------------------------------------------------------------------------

const toggle = document.getElementById('ai-first-toggle');
const labelYou = document.getElementById('label-you');
const labelAi = document.getElementById('label-ai');
toggle.addEventListener('change', () => {
    labelYou.classList.toggle('active', !toggle.checked);
    labelAi.classList.toggle('active', toggle.checked);
});

newGameBtn.addEventListener('click', () => {
    if (gameActive) {
        // Resign — notify server to free the slot
        if (gameToken) {
            fetch('/api/game', {
                method: 'DELETE',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ token: gameToken })
            }).catch(() => {});
            gameToken = null;
            sessionStorage.removeItem('gameToken');
        }
        endGame();
        clearDebug();
        setStatus('Change settings, then click New Game');
        return;
    }
    newGame(toggle.checked);
});

// ---------------------------------------------------------------------------
// Debug panel
// ---------------------------------------------------------------------------

const dbgToggle = document.getElementById('debug-mode');
const dbgPanel = document.getElementById('debug-panel');
let dbgOn = false;
dbgToggle.addEventListener('change', () => {
    dbgOn = dbgToggle.checked;
    dbgPanel.style.display = dbgOn ? 'block' : 'none';
});

function showDebug(data) {
    if (!dbgOn) return;
    document.getElementById('dbg-depth').textContent = depthSelect.value;
    const win = data['immediate-win'] ?? data.immediateWin;
    const blk = data['immediate-block'] ?? data.immediateBlock;
    document.getElementById('dbg-win').textContent = win != null ? `Col ${win + 1}` : 'None';
    document.getElementById('dbg-block').textContent = blk != null ? `Col ${blk + 1}` : 'None';
    const scores = data['ai-scores'] || data.aiScores || [];
    const chose = data['ai-move'] ?? data.aiMove;
    const els = document.querySelectorAll('#dbg-scores .debug-col .score');
    const fmt = (n) => {
        if (n === null) return 'X';
        if (n === 50000) return 'WIN';
        if (n === -50000) return 'LOSS';
        if (n === 49000) return '49k';
        if (n === -49000) return '-49k';
        if (Math.abs(n) >= 10000) return (n > 0 ? '' : '-') + Math.round(Math.abs(n) / 1000) + 'k';
        if (Number.isInteger(n)) return String(n);
        return n.toFixed(1);
    };
    scores.forEach(([c, s]) => {
        if (els[c]) {
            if (s === null) {
                els[c].textContent = 'X';
                els[c].className = 'score x';
            } else {
                els[c].textContent = fmt(s);
                els[c].className = 'score' + (c === chose ? ' best' : s > 0 ? ' pos' : s < 0 ? ' neg' : '');
            }
        }
    });
    document.getElementById('dbg-chose').textContent = chose != null ? `Column ${chose + 1}` : '-';
}

function clearDebug() {
    ['dbg-depth', 'dbg-win', 'dbg-block', 'dbg-chose'].forEach(id =>
        document.getElementById(id).textContent = '-'
    );
    document.querySelectorAll('#dbg-scores .score').forEach(e => {
        e.textContent = '-';
        e.className = 'score';
    });
}

// ---------------------------------------------------------------------------
// Session persistence & cleanup
// ---------------------------------------------------------------------------

// Free the game slot when the user closes/refreshes the tab
window.addEventListener('beforeunload', () => {
    if (gameToken) {
        const blob = new Blob(
            [JSON.stringify({ token: gameToken })],
            { type: 'application/json' }
        );
        navigator.sendBeacon('/api/game', blob);
    }
});

// ---------------------------------------------------------------------------
// Initialise
// ---------------------------------------------------------------------------

initBoard();
refreshSlotCount();
