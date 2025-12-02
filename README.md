# Connect-4 Lisp Heuristic Player - Web Dashboard

A web interface for the Connect-4 Heuristic AI, powered by Common Lisp.

<img src="images/screenshot-1.jpg" alt="Connect 4 Screenshot" width="600">

## Overview

This project wraps the original [Connect4-Heuristic-Player](https://github.com/famesjranko/Connect4-Heuristic-Player) Lisp implementation with a modern web interface. The original game logic, minimax algorithm, and heuristic evaluation remain **completely unchanged** - we've simply added an HTTP API layer on top.

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
│     Original Lisp Code      │
│  ├─ minimax.lisp            │  ← Minimax with α-β pruning  
│  ├─ connect-4.lisp          │  ← Game logic & board
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

### Using Docker (Recommended)

```bash
# Build and run
docker compose up --build

# Or just docker
docker build -t connect4-lisp .
docker run -p 8080:8080 connect4-lisp
```

Open http://localhost:8080 in your browser.

### Docker Compose with Replicas

```yaml
version: '3.8'
services:
  connect4:
    build: .
    deploy:
      replicas: 3
    ports:
      - "8080:8080"
```

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

## Features

- **Horizontally scalable** - no server-side session state
- **Theme picker** - 8 themes from minimal to retro arcade
- **Keyboard support**: Press 1-7 to drop pieces, arrow keys to select, Enter to confirm
- **Adjustable AI difficulty** (depth 1-10)
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
│   └── screenshot.jpg       # Game screenshot
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

The AI uses minimax with alpha-beta pruning and a custom heuristic. You can adjust the search depth:

- **Depth 1-2**: Easy (fast, not very strategic)
- **Depth 3-4**: Medium (good balance, default is 4)
- **Depth 5-6**: Hard (slower, very strategic)
- **Depth 7+**: Very hard (may be slow on complex boards)

The heuristic evaluates:
- **Positional value**: Center columns weighted higher
- **Threat detection**: Imminent win/loss detection
- **Defensive weighting**: Tuned to prioritize blocking

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
- **Frontend**: Vanilla HTML/CSS/JS (no frameworks)
- **Container**: Debian Bookworm slim base (non-root)

## Security & Performance

- Stateless design eliminates session-related vulnerabilities
- No server-side memory accumulation
- Non-root container execution
- Cached DOM references in frontend
- Input validation on all endpoints

## Original Project

The Lisp game engine is from [famesjranko/Connect4-Heuristic-Player](https://github.com/famesjranko/Connect4-Heuristic-Player) by Andrew McDonald.

The minimax implementation is based on code from "Artificial Intelligence" by Elaine Rich and Kevin Knight (McGraw Hill, 1991).
