<p align="center"> <img alt="Space Station 15" width="880" height="300" src="https://raw.githubusercontent.com/Execyte/asset-dump/refs/heads/main/svg/SS15longBGP.svg" /></p>

[![Join our Discord server](https://img.shields.io/badge/join_our-Discord_server-5865F2?logo=discord&logoColor=white)](https://discord.gg/qW8bHkncrb) [![Powered by Haskell](https://img.shields.io/badge/powered_by-Haskell-5D4F85?logo=haskell&logoColor=white)](https://haskell.org)

Space Station 15 is a remake of Space Station 13 built from scratch in Haskell, which adds its own gimmicks to the game while staying true to the original's style and design.

# Building
## Prerequisites
- [Git](https://git-scm.com/downloads)
- [Stack](https://docs.haskellstack.org/en/stable/#how-to-install-stack)
## Windows
**NOTE:** This method of building currently does not work.

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
    
    This will produce two executables: `space-station15-client` and `space-station15-server`. Run them with the command:

    ```
    stack exec [executable name]
    ```

## macOS (not supported)
macOS is not supported at this moment. Help with building on macOS would be appreciated.

## Linux
1. Install development libraries for SDL2 and OpenAL with your package manager.
2. To build, run:
    
    ```
    stack build
    ```
    
    This will produce two executables: `space-station15-client` and `space-station15-server`. Run them with the command:

    ```
    stack exec [executable name]
    ```
