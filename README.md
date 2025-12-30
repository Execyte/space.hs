<p align="center"> <img alt="Space.hs" width="880" height="200" src="https://raw.githubusercontent.com/Techgineers7725/asset-dump/refs/heads/main/svg/Space-hs-banner.svg"/></p>

[![Join our Discord server](https://img.shields.io/badge/join_our-Discord_server-5865F2?logo=discord&logoColor=white)](https://discord.gg/pPUXAZMMYN) [![Powered by Haskell](https://img.shields.io/badge/powered_by-Haskell-5D4F85?logo=haskell&logoColor=white)](https://haskell.org)

Space.hs is a round-based game about surviving and maintaing a space station amongst many kinds of threats.

# Building
## Prerequisites
- [Git](https://git-scm.com/downloads)
- [Stack](https://docs.haskellstack.org/en/stable/#how-to-install-stack)

## Windows
1. Install SDL2 and OpenAL using this command:
    
    ```
    stack exec -- pacman -S mingw-w64-x86_64-pkgconf mingw-w64-x86_64-SDL2 mingw-w64-x86_64-openal
    ```
    
    **NOTE:** If you already had Stack installed, you might want to update the package index first with the following commands:

    ```
    stack exec -- pacman -Sy msys2-keyring
    stack exec -- pacman -Syu
    ```
3. To build, run:

    ```
    stack build
    ```
    
    This will produce two executables: `space-hs-client` and `space-hs-server`. Run them with the command:

    ```
    stack exec [executable name]
    ```

## macOS
macOS building works similarly to Linux, please refer to stack docs for further information.

## Linux
1. Install development libraries for SDL2 and OpenAL with your package manager.
2. To build, run:
    
    ```
    stack build
    ```
    
    This will produce two executables: `space-hs-client` and `space-hs-server`. Run them with the command:

    ```
    stack exec [executable name]
    ```

# Hoogle
1. To build Hoogle, run:
```
stack hoogle -- generate --local
```

2. To run the Hoogle server, run:
```
stack hoogle -- server --local --port=8080
```

# Haddock
To auto-generate documentation for the game, you can use the following command (which will also build the game):
```
stack haddock --haddock-executables
```
