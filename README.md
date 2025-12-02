# Connect-4 Lisp AI - Stateless Version

A Connect-4 game with a Lisp-based AI opponent, served via a web interface.

**This is the horizontally-scalable stateless version** - game state is stored client-side and sent with each request, allowing multiple server instances behind a load balancer.

## Features

- **Horizontally Scalable**: No server-side session state
- **Multiple Themes**: 8 beautiful themes to choose from
- **Smart AI**: Minimax with alpha-beta pruning
- **Adjustable Difficulty**: Search depth 1-10
- **Debug Mode**: See AI's move analysis

## Architecture

### Stateless Design

Unlike traditional session-based designs:
- **Client stores board state** in JavaScript
- **Each request includes full board state** via POST body
- **No game IDs or server-side storage**
- **Any instance can handle any request**

```
┌─────────────────────────────┐
│   Browser (stores board)    │
└──────────────┬──────────────┘
               │ POST {board: [...]}
┌──────────────▼──────────────┐
│      Load Balancer          │
├──────────┬──────────────────┤
│ Instance │ Instance │ ...   │  ← Any instance can handle any request
└──────────┴──────────────────┘
```

### API

```
GET  /api/health                        - Health check
GET  /api/new-game                      - Get empty board
GET  /api/new-game-ai-first?depth=N     - Get board with AI's first move
POST /api/move?column=N&depth=D         - Make move (board in POST body)
```

### Request Format

```javascript
// POST /api/move?column=3&depth=4
// Content-Type: application/json
{
  "board": [
    [null, null, null, null, null, null],  // Column 0
    [null, null, null, null, null, null],  // Column 1
    [null, null, null, null, null, null],  // Column 2
    ["x", null, null, null, null, null],   // Column 3 - has a player piece
    ["o", null, null, null, null, null],   // Column 4 - has an AI piece
    [null, null, null, null, null, null],  // Column 5
    [null, null, null, null, null, null]   // Column 6
  ]
}
```

### Response Format

```javascript
{
  "board": [[...], ...],           // Updated board state
  "status": "ongoing",             // ongoing | player_wins | ai_wins | draw
  "ai-move": 3,                    // Column AI played (0-6)
  "ai-scores": [[0, 12.5], ...],   // Debug: score for each column
  "evaluations": 1523,             // Debug: positions evaluated
  "message": "Your turn!"
}
```

## Deployment

### Railway / Render / Fly.io

1. Push to GitHub
2. Connect repo to platform
3. Deploy - auto-detects Dockerfile
4. Scale to multiple instances ✓

### Docker

```bash
docker build -t connect4-stateless .
docker run -p 8080:8080 connect4-stateless
```

### Docker Compose (with replicas)

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

## Scaling Comparison

| Feature | Original (Stateful) | This Version (Stateless) |
|---------|---------------------|--------------------------|
| Server-side storage | Hash table | None |
| Horizontal scaling | ❌ No | ✅ Yes |
| Sticky sessions | Required | Not needed |
| Request handling | Instance-specific | Any instance |
| Memory per game | ~1KB | 0 |

## Tech Stack

- **Backend**: Common Lisp (SBCL) + Hunchentoot
- **AI**: Minimax with α-β pruning
- **Frontend**: Vanilla HTML/CSS/JavaScript
- **Container**: Docker

## Themes

- Modern (clean dark)
- Arcade (retro neon)
- Terminal (Lisp REPL style)
- Neon (cyberpunk glow)
- Paper (light, hand-drawn)
- Midnight (deep blue)
- Sunset (warm gradients)
- Hacker (Matrix style)

## Credits

Original game engine by Andrew McDonald
