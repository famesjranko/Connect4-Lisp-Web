# Connect-4 Lisp Heuristic Player - Web Dashboard

<img src="images/screenshot-1.jpg" alt="Connect 4 Screenshot" width="600">

## Overview

This project adds a modern web interface to my original [Connect4-Heuristic-Player](https://github.com/famesjranko/Connect4-Heuristic-Player) Lisp implementation from university. The original heuristic evaluation logic is preserved, while the underlying data structures and search have been optimised for performance (array-based board, transposition table, parallel search).

### Architecture

```
┌─────────────────────────────┐
│   Browser (stores board)    │  ← Game state lives here
└──────────────┬──────────────┘
               │ POST {board: [...]}
┌──────────────▼──────────────┐
│      Load Balancer          │
├─────────┬─────────┬─────────┤
│ Instance│ Instance│ Instance│  ← Any instance handles any request
└────┬────┴────┬────┴────┬────┘
     │         │         │
┌────▼─────────▼─────────▼────┐
│     Lisp AI Engine           │
│  ├─ minimax.lisp            │  ← α-β pruning + transposition table
│  ├─ connect-4.lisp          │  ← Game logic + Zobrist hashing
│  └─ heuristic.lisp          │  ← AI evaluation function
└─────────────────────────────┘
```

### Stateless Design

Unlike traditional session-based game servers:
- **Client stores board state** in JavaScript
- **Each request includes full board state** via POST body
- **No game IDs or server-side storage**
- **Any instance can handle any request**

This enables true horizontal scaling without sticky sessions.

## Quick Start

### Using Docker (Local)

```bash
# Build and run
docker compose up --build

# Or just docker
docker build -t connect4-lisp .
docker run -p 8080:8080 connect4-lisp
```

Open http://localhost:8080 in your browser.

### Cloud Deployment (Railway / Render / Fly.io / Cloud Run)

1. Push to GitHub
2. Connect repo to platform
3. Deploy - auto-detects Dockerfile
4. Scale to multiple instances ✓

No sticky sessions or shared state required.

### Running Locally (requires SBCL)

```bash
# Install SBCL and Quicklisp first, then:
sbcl --load web-server.lisp
```

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PORT` | 8080 | Server port |
| `LISP_AI_DEPTH` | 3 | Default AI search depth |
| `LISP_AI_MAX_DEPTH` | 6 | Maximum allowed search depth |
| `LISP_AI_THREADS` | 4 | Worker threads for parallel search |

## Features

- **Horizontally scalable** - no server-side session state
- **Theme picker** - 8 themes from minimal to retro arcade
- **Keyboard support**: Press 1-7 to drop pieces, arrow keys to select, Enter to confirm
- **Adjustable AI difficulty** (depth 1-8)
- **Debug mode** - see AI's move analysis and scores
- **Winning line highlighting** when game ends
- **Non-root Docker** container for security
- **Theme persistence** - your preference is saved to localStorage

## Themes

The UI supports 8 unique themes, each in its own standalone HTML file:

```
static/
├── index.html      # Redirects to modern.html
├── modern.html     # Clean, minimal dark theme
├── arcade.html     # Retro arcade with glows and 3D board
├── terminal.html   # CRT terminal / Lisp REPL aesthetic
├── neon.html       # Cyberpunk pink/cyan neon
├── paper.html      # Light notebook/sketch aesthetic
├── midnight.html   # Space theme with stars and nebula
├── sunset.html     # Warm orange/pink gradients
└── hacker.html     # Matrix digital rain effect
```

Add new themes by creating additional HTML files in `static/`. Each theme is fully self-contained.

## Project Structure

```
connect4-lisp/
├── src/                     # Original Lisp files
│   ├── minimax.lisp         # Minimax algorithm with α-β pruning
│   ├── connect-4.lisp       # Game board, moves, win detection
│   └── heuristic.lisp       # AI heuristic evaluation
├── static/                  # 8 theme files (see Themes section)
│   ├── index.html           # Redirect to default theme
│   ├── modern.html          # Modern theme
│   ├── arcade.html          # Arcade theme
│   ├── terminal.html        # Terminal theme
│   ├── neon.html            # Neon theme
│   ├── paper.html           # Paper theme
│   ├── midnight.html        # Midnight theme
│   ├── sunset.html          # Sunset theme
│   └── hacker.html          # Hacker theme
├── images/
│   └── screenshot-1.jpg     # Game screenshot
├── web-server.lisp          # HTTP API layer (Hunchentoot)
├── Dockerfile
├── docker-compose.yml
└── README.md
```

## API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/new-game` | GET | Get empty board (player first) |
| `/api/new-game-ai-first?depth=N` | GET | Get board with AI's first move |
| `/api/move?column=N&depth=D` | POST | Make a move (board in POST body) |
| `/api/debug?depth=N` | POST | Evaluate board without moving |
| `/api/health` | GET | Health check |

### Request Format

```javascript
// POST /api/move?column=3&depth=4
// Content-Type: application/json
{
  "board": [
    [null, null, null, null, null, null],  // Column 0 (bottom to top)
    [null, null, null, null, null, null],  // Column 1
    [null, null, null, null, null, null],  // Column 2
    ["X", null, null, null, null, null],   // Column 3 - player piece at bottom
    ["O", null, null, null, null, null],   // Column 4 - AI piece at bottom
    [null, null, null, null, null, null],  // Column 5
    [null, null, null, null, null, null]   // Column 6
  ]
}
```

### Response Format

```json
{
  "board": [[null,null,null,null,null,null], ...],
  "status": "ongoing",
  "aiMove": 3,
  "aiScores": [[0, 12], [1, 8], [2, 15], ...],
  "evaluations": 1234,
  "immediateWin": null,
  "immediateBlock": 2,
  "winningCells": [[3,0], [3,1], [3,2], [3,3]],
  "message": "Your turn"
}
```

### Health Check Response

```json
{
  "status": "ok",
  "version": "stateless"
}
```

## AI Configuration

The AI uses minimax with alpha-beta pruning, a transposition table, and parallel root-level search. You can adjust the search depth:

- **Depth 1-2**: Easy (fast, not very strategic)
- **Depth 3-4**: Medium (good balance, default is 4)
- **Depth 5-6**: Hard (very strategic)
- **Depth 7-8**: Very hard (strongest play, sub-second response)

The heuristic evaluates:
- **Positional value**: Center columns weighted higher via a strategy value map
- **Threat detection**: Imminent win/loss detection
- **Defensive weighting**: Tuned to prioritize blocking over risky attacks at shallow depths
- **Diagonal strategy**: Extra weight for diagonal win potential

### Performance Optimisations

- **Array-based board**: 2D array with O(1) cell access replaces nested lists
- **Transposition table**: Zobrist hashing with 1M-entry cache avoids re-evaluating positions reached via different move orders
- **Parallel search**: Root-level moves evaluated concurrently via lparallel (first move searched sequentially to establish alpha-beta bound)
- **Reduced allocations**: Move generation returns column indices instead of full board copies; intermediate list allocations eliminated

## Controls

### Mouse
- Click a column to drop a piece

### Keyboard
- **1-7**: Drop piece in column 1-7
- **←/→**: Select column
- **Enter/Space**: Drop piece in selected column
- **N**: Start new game

## Technical Details

- **Lisp Implementation**: SBCL (Steel Bank Common Lisp)
- **Web Server**: Hunchentoot
- **JSON Handling**: cl-json
- **Parallelism**: lparallel
- **Frontend**: Vanilla HTML/CSS/JS (no frameworks)
- **Container**: Debian Bookworm slim base (non-root)

## Security & Performance

- Stateless design eliminates session-related vulnerabilities
- No server-side memory accumulation
- Per-request transposition tables for thread-safe concurrent handling
- Non-root container execution
- Cached DOM references in frontend
- Input validation on all endpoints (board shape, column range, depth clamping)

## Credits

Original Lisp game engine from my university project: [Connect4-Heuristic-Player](https://github.com/famesjranko/Connect4-Heuristic-Player)

Minimax algorithm based on code from *Artificial Intelligence* by Elaine Rich and Kevin Knight (McGraw Hill, 1991).
