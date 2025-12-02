# Connect-4 Heuristic Player - Web Edition

A web interface for the Connect-4 Heuristic AI, powered by Common Lisp.

![Connect 4 Screenshot](images/screenshot-1.jpg)

## Overview

This project wraps the original [Connect4-Heuristic-Player](https://github.com/famesjranko/Connect4-Heuristic-Player) Lisp implementation with a modern web interface. The original game logic, minimax algorithm, and heuristic evaluation remain **completely unchanged** - we've simply added an HTTP API layer on top.

### Architecture

```
┌─────────────────────┐
│   Browser (HTML/JS) │
└──────────┬──────────┘
           │ HTTP/JSON
┌──────────▼──────────┐
│   Hunchentoot       │  ← Web server layer (web-server.lisp)
│   (CL HTTP Server)  │
└──────────┬──────────┘
           │ Function calls
┌──────────▼──────────┐
│  Original Lisp Code │
│  ├─ minimax.lisp    │  ← Minimax with α-β pruning  
│  ├─ connect-4.lisp  │  ← Game logic & board
│  └─ heuristic.lisp  │  ← AI evaluation function
└─────────────────────┘
```

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

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `LISP_AI_DEPTH` | 4 | Default AI search depth (1-6) |

### Running Locally (requires SBCL)

```bash
# Install SBCL and Quicklisp first, then:
sbcl --load web-server.lisp
```

## Features

- **Theme picker** - Choose between Modern (clean minimal) and Arcade (retro glow) themes
- **Keyboard support**: Press 1-7 to drop pieces, arrow keys to select, Enter to confirm
- **Adjustable AI difficulty** (depth 1-6)
- **Winning line highlighting** when game ends
- **Per-game settings** - multiple concurrent games supported
- **Automatic cleanup** of abandoned games (1 hour timeout)
- **Thread-safe** game state management
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
| `/api/new-game` | GET | Start new game (player first) |
| `/api/new-game-ai-first` | GET | Start new game (AI first) |
| `/api/move?game_id=X&column=N` | GET | Make a move (column 0-6) |
| `/api/set-depth?game_id=X&depth=N` | GET | Set AI depth for game (1-6) |
| `/api/health` | GET | Health check with stats |

### Example Response

```json
{
  "board": [[null,null,null,null,null,null], ...],
  "status": "ongoing",
  "aiMove": 3,
  "evaluations": 1234,
  "winningCells": [[3,0], [3,1], [3,2], [3,3]],
  "message": "Your turn"
}
```

### Health Check Response

```json
{
  "status": "ok",
  "activeGames": 5,
  "uptimeSeconds": 3600,
  "defaultDepth": 4
}
```

## AI Configuration

The AI uses minimax with alpha-beta pruning and a custom heuristic. You can adjust the search depth:

- **Depth 1-2**: Easy (fast, not very strategic)
- **Depth 3-4**: Medium (good balance, default is 4)
- **Depth 5-6**: Hard (slower, very strategic)

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

## Original Project

The Lisp game engine is from [famesjranko/Connect4-Heuristic-Player](https://github.com/famesjranko/Connect4-Heuristic-Player) by Andrew McDonald.

The minimax implementation is based on code from "Artificial Intelligence" by Elaine Rich and Kevin Knight (McGraw Hill, 1991).

## Technical Details

- **Lisp Implementation**: SBCL (Steel Bank Common Lisp)
- **Web Server**: Hunchentoot
- **JSON Handling**: cl-json
- **Threading**: bordeaux-threads
- **Frontend**: Vanilla HTML/CSS/JS (no frameworks)
- **Container**: Debian Bookworm slim base (non-root)

## Security & Performance

- Thread-safe game state with locks
- Automatic game cleanup (1 hour expiry)
- High-entropy game IDs
- Non-root container execution
- Cached DOM references in frontend
- Per-game depth settings (concurrent games don't affect each other)

## License

Original Lisp code may be freely copied and used for educational or research purposes.
