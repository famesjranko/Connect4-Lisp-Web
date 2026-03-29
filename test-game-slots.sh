#!/bin/bash
# Test suite for Redis-backed game slot system
BASE="http://localhost:8080"
PASS=0
FAIL=0
TOKENS=()

# Helpers
red()   { echo -e "\033[31m$1\033[0m"; }
green() { echo -e "\033[32m$1\033[0m"; }
bold()  { echo -e "\033[1m$1\033[0m"; }

assert_eq() {
    local label="$1" expected="$2" actual="$3"
    if [ "$expected" = "$actual" ]; then
        green "  PASS: $label"
        PASS=$((PASS+1))
    else
        red "  FAIL: $label (expected '$expected', got '$actual')"
        FAIL=$((FAIL+1))
    fi
}

assert_contains() {
    local label="$1" expected="$2" actual="$3"
    if echo "$actual" | grep -q "$expected"; then
        green "  PASS: $label"
        PASS=$((PASS+1))
    else
        red "  FAIL: $label (expected to contain '$expected', got '$actual')"
        FAIL=$((FAIL+1))
    fi
}

# =========================================================================
bold "=== Test 1: Health check ==="
RESP=$(curl -s "$BASE/api/health")
assert_contains "health status ok" '"status":"ok"' "$RESP"
assert_contains "redis connected" '"redis":"connected"' "$RESP"
assert_contains "active games 0" '"activeGames":0' "$RESP"
assert_contains "max games 4" '"maxGames":4' "$RESP"

# =========================================================================
bold "=== Test 2: New game (player first) ==="
RESP=$(curl -s -X POST -H "Content-Type: application/json" \
    -d '{"depth":3}' "$BASE/api/new-game")
TOKEN1=$(echo "$RESP" | python3 -c "import sys,json; print(json.load(sys.stdin)['token'])")
assert_contains "has token" '"token"' "$RESP"
assert_contains "status ongoing" '"status":"ongoing"' "$RESP"
assert_contains "turn x" '"turn":"x"' "$RESP"
assert_contains "message your turn" '"Your turn!"' "$RESP"
TOKENS+=("$TOKEN1")
echo "  Token: $TOKEN1"

# =========================================================================
bold "=== Test 3: Health check shows 1 active game ==="
RESP=$(curl -s "$BASE/api/health")
assert_contains "1 active game" '"activeGames":1' "$RESP"

# =========================================================================
bold "=== Test 4: Make a move (player drops in col 3) ==="
RESP=$(curl -s -X POST -H "Content-Type: application/json" \
    -d "{\"token\":\"$TOKEN1\",\"column\":3}" "$BASE/api/move")
assert_contains "status ongoing" '"status":"ongoing"' "$RESP"
assert_contains "has ai move" '"aiMove"' "$RESP"
assert_contains "has evaluations" '"evaluations"' "$RESP"
assert_contains "message your turn" '"Your turn!"' "$RESP"

# =========================================================================
bold "=== Test 5: Make another move (col 3 again - stacking) ==="
RESP=$(curl -s -X POST -H "Content-Type: application/json" \
    -d "{\"token\":\"$TOKEN1\",\"column\":3}" "$BASE/api/move")
assert_contains "status ongoing or terminal" '"status"' "$RESP"

# =========================================================================
bold "=== Test 6: New game (AI first) ==="
RESP=$(curl -s -X POST -H "Content-Type: application/json" \
    -d '{"depth":2,"aiFirst":true}' "$BASE/api/new-game")
TOKEN2=$(echo "$RESP" | python3 -c "import sys,json; print(json.load(sys.stdin)['token'])")
assert_contains "has token" '"token"' "$RESP"
assert_contains "has ai move" '"aiMove"' "$RESP"
assert_contains "has evaluations" '"evaluations"' "$RESP"
assert_contains "ai moved first" '"AI moved first!"' "$RESP"
TOKENS+=("$TOKEN2")
echo "  Token: $TOKEN2"

# =========================================================================
bold "=== Test 7: Health check shows 2 active games ==="
RESP=$(curl -s "$BASE/api/health")
assert_contains "2 active games" '"activeGames":2' "$RESP"

# =========================================================================
bold "=== Test 8: Invalid token returns 404 ==="
CODE=$(curl -s -o /dev/null -w "%{http_code}" -X POST -H "Content-Type: application/json" \
    -d '{"token":"nonexistent-token","column":3}' "$BASE/api/move")
assert_eq "404 for invalid token" "404" "$CODE"

# =========================================================================
bold "=== Test 9: Invalid column returns 400 ==="
CODE=$(curl -s -o /dev/null -w "%{http_code}" -X POST -H "Content-Type: application/json" \
    -d "{\"token\":\"$TOKEN1\",\"column\":99}" "$BASE/api/move")
assert_eq "400 for invalid column" "400" "$CODE"

# =========================================================================
bold "=== Test 10: GET on /api/new-game returns 405 ==="
CODE=$(curl -s -o /dev/null -w "%{http_code}" "$BASE/api/new-game")
assert_eq "405 for GET new-game" "405" "$CODE"

# =========================================================================
bold "=== Test 11: Fill remaining slots (create 2 more games) ==="
RESP3=$(curl -s -X POST -H "Content-Type: application/json" \
    -d '{"depth":2}' "$BASE/api/new-game")
TOKEN3=$(echo "$RESP3" | python3 -c "import sys,json; print(json.load(sys.stdin)['token'])")
TOKENS+=("$TOKEN3")
assert_contains "game 3 created" '"token"' "$RESP3"

RESP4=$(curl -s -X POST -H "Content-Type: application/json" \
    -d '{"depth":2}' "$BASE/api/new-game")
TOKEN4=$(echo "$RESP4" | python3 -c "import sys,json; print(json.load(sys.stdin)['token'])")
TOKENS+=("$TOKEN4")
assert_contains "game 4 created" '"token"' "$RESP4"

# =========================================================================
bold "=== Test 12: 5th game returns 503 (slots full) ==="
RESP=$(curl -s -X POST -H "Content-Type: application/json" \
    -d '{"depth":2}' "$BASE/api/new-game")
CODE=$(curl -s -o /dev/null -w "%{http_code}" -X POST -H "Content-Type: application/json" \
    -d '{"depth":2}' "$BASE/api/new-game")
assert_eq "503 when slots full" "503" "$CODE"
assert_contains "slots_full error" '"slots_full"' "$RESP"

# =========================================================================
bold "=== Test 13: Health check shows 4 active games ==="
RESP=$(curl -s "$BASE/api/health")
assert_contains "4 active games" '"activeGames":4' "$RESP"

# =========================================================================
# Wait for rate limit window to reset after rapid slot-filling
sleep 11

# =========================================================================
bold "=== Test 14: Resign a game (DELETE /api/game) ==="
RESP=$(curl -s -X DELETE -H "Content-Type: application/json" \
    -d "{\"token\":\"$TOKEN3\"}" "$BASE/api/game")
assert_contains "game ended" '"Game ended."' "$RESP"

# =========================================================================
bold "=== Test 15: Slot freed — can create new game ==="
RESP=$(curl -s "$BASE/api/health")
assert_contains "3 active games" '"activeGames":3' "$RESP"

RESP5=$(curl -s -X POST -H "Content-Type: application/json" \
    -d '{"depth":2}' "$BASE/api/new-game")
TOKEN5=$(echo "$RESP5" | python3 -c "import sys,json; print(json.load(sys.stdin)['token'])")
TOKENS+=("$TOKEN5")
assert_contains "new game after resign" '"token"' "$RESP5"

# =========================================================================
bold "=== Test 16: POST to /api/game also works (sendBeacon compat) ==="
RESP=$(curl -s -X POST -H "Content-Type: application/json" \
    -d "{\"token\":\"$TOKEN5\"}" "$BASE/api/game")
assert_contains "POST resign works" '"Game ended."' "$RESP"

# =========================================================================
bold "=== Test 17: Debug endpoint ==="
RESP=$(curl -s -X POST -H "Content-Type: application/json" \
    -d "{\"token\":\"$TOKEN1\"}" "$BASE/api/debug")
assert_contains "debug has ai-scores" '"aiScores"' "$RESP"
assert_contains "debug has depth" '"depth"' "$RESP"

# =========================================================================
bold "=== Test 18: Token length validation ==="
LONG_TOKEN=$(python3 -c "print('a'*100)")
CODE=$(curl -s -o /dev/null -w "%{http_code}" -X POST -H "Content-Type: application/json" \
    -d "{\"token\":\"$LONG_TOKEN\",\"column\":3}" "$BASE/api/move")
assert_eq "reject long token" "404" "$CODE"

# =========================================================================
# Wait for rate limit window to reset
sleep 11

# =========================================================================
bold "=== Test 19: Play a full sequence of moves ==="
# Create fresh game
RESP=$(curl -s -X POST -H "Content-Type: application/json" \
    -d '{"depth":1}' "$BASE/api/new-game")
TGAME=$(echo "$RESP" | python3 -c "import sys,json; print(json.load(sys.stdin)['token'])")
# Clean up a slot first
curl -s -X DELETE -H "Content-Type: application/json" \
    -d "{\"token\":\"$TOKEN4\"}" "$BASE/api/game" > /dev/null

# Play several moves
for col in 0 1 2 0 1 2; do
    RESP=$(curl -s -X POST -H "Content-Type: application/json" \
        -d "{\"token\":\"$TGAME\",\"column\":$col}" "$BASE/api/move")
    STATUS=$(echo "$RESP" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('status','error'))" 2>/dev/null)
    if [ "$STATUS" != "ongoing" ]; then
        echo "  Game ended with status: $STATUS"
        break
    fi
done
assert_contains "game progressed" '"status"' "$RESP"

# =========================================================================
bold "=== Test 20: Rate limiting (429) ==="
# Rapid-fire requests to trigger rate limit (limit is 10/10s)
for i in $(seq 1 12); do
    curl -s -o /dev/null -X POST -H "Content-Type: application/json" \
        -d '{"depth":1}' "$BASE/api/new-game"
done
CODE=$(curl -s -o /dev/null -w "%{http_code}" -X POST -H "Content-Type: application/json" \
    -d '{"depth":1}' "$BASE/api/new-game")
assert_eq "429 after rate limit exceeded" "429" "$CODE"
# Wait for window to reset
sleep 11

# =========================================================================
bold "=== Test 21: Expired token after resign ==="
# TOKEN3 was resigned earlier
CODE=$(curl -s -o /dev/null -w "%{http_code}" -X POST -H "Content-Type: application/json" \
    -d "{\"token\":\"$TOKEN3\",\"column\":3}" "$BASE/api/move")
assert_eq "404 for resigned game token" "404" "$CODE"

# =========================================================================
# Cleanup remaining games
for t in "${TOKENS[@]}" "$TGAME"; do
    curl -s -X DELETE -H "Content-Type: application/json" \
        -d "{\"token\":\"$t\"}" "$BASE/api/game" > /dev/null 2>&1
done

# =========================================================================
echo ""
bold "========================================="
bold "Results: $PASS passed, $FAIL failed"
bold "========================================="

if [ $FAIL -gt 0 ]; then
    exit 1
fi
