[![progress-banner](https://backend.codecrafters.io/progress/interpreter/747d192d-bfbc-44ab-bf5d-be8395438404)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)

This is my solution for the
["Build Your Own Build your own Interpreter" Challenge](https://app.codecrafters.io/courses/interpreter/overview) from CodeCrafters.

### Some useful knowledge

I use git for the testing but using three commands all the time was tiring, such is the case I created an alias instead.

```bash
git config --global alias.codecrafters '!f() { git add . && git commit --allow-empty -m "${1:-pass stage}" && git push origin master; }; f'
```

Needing only to:

```bash
git codecrafters
# And possible add a commit message
git codecrafters "feat: hire me"
```
