<p align="center"> <img alt="Space.hs" width="880" height="200" src="https://raw.githubusercontent.com/Techgineers7745/asset-dump/refs/heads/main/svg/Space-hs-banner.svg"/></p>

[![Join our Discord server](https://img.shields.io/badge/join_our-Discord_server-5865F2?logo=discord&logoColor=white)](https://discord.gg/pPUXAZMMYN) [![Powered by Haskell](https://img.shields.io/badge/powered_by-Haskell-5D4F85?logo=haskell&logoColor=white)](https://haskell.org)

Space.hs is a round-based game about surviving and maintaing a space station amidst many kinds of threats wanting to destroy it.

# Building
Prerequisites before building:
- [Git](https://git-scm.com/downloads)
- [Stack](https://docs.haskellstack.org/en/stable/#how-to-install-stack)

All you have to do to build is just to run `stack build` in the top level directory of the project. That produces 2 executables: `client` and `server`, which you can run with `stack exec [executable-name]`. More platform-specific build instructions below.

### Windows
Make sure to run these commands before installing the development libraries:

```
stack exec -- pacman -Sy msys2-keyring
stack exec -- pacman -Syu
```

Now you can do this:

```
stack exec -- pacman -S mingw-w64-x86_64-pkgconf mingw-w64-x86_64-SDL2 mingw-w64-x86_64-openal
```

### Linux
Install the necessary development libraries for SDL2 and OpenAL with your package manager.

# Hoogle
Hoogle is a tool that allows you to search for functions using their name and/or type definitions.

build Hoogle using `stack hoogle -- generate --local` then run the server with `stack hoogle -- server --local --port=8080` and access [it](localhost:8080) in your browser.

# Haddock
If you want to generate code documentation, you can use the stack haddock command like so: `stack haddock --haddock-executables`, which will additionally also build the game if you haven't done it beforehand.
