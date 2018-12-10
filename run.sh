#!/usr/bin/env bash
# Running API
cd api/
echo "main" | stack ghci --allow-different-user &

# Running Front-end
cd ../dontmix/
elm-live src/App.elm --pushstate &

# Running DEVD
cd ..
devd -X /api/=http://localhost:3000 /=http://localhost:8000 -p 8080
