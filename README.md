<p align="center"> <img alt="Space Station 15" width="880" height="300" src="https://raw.githubusercontent.com/Execyte/asset-dump/refs/heads/main/svg/SS15longBGP.svg"/></p>

[![Join our Discord server](https://img.shields.io/badge/join_our-Discord_server-5865F2?logo=discord&logoColor=white)](https://discord.gg/qW8bHkncrb) [![Powered by Haskell](https://img.shields.io/badge/powered_by-Haskell-5D4F85?logo=haskell&logoColor=white)](https://haskell.org)

Space Station 15 is a game based around the fundamentals of Space Station 13 with custom additions added on top and a whole bunch of things outright changed. This is a whole separate project, not a full-on, accurate recreation.

# Motivation

My motivation to start working on space station 15 stems from the main fact that I really appreciate how the original SS13 game was build, but a lot of the ideas and thoughts I've wanted to recreate in the genre of the game just simply did not fit what the game was supposed to be. But still, why a whole 'nother game, why not a fork? This is actually due to SS14, the original SS13 game recreated in C#. It has inspired me a little bit, but I'd much more appreciate if I could build my vision of space station 13 in the tools that I like and I'm comfortable using.

Now that this is all out of the way, I don't expect maintainers and I don't expect a whole lot of players. Contribute to the project if u wish, join the discord to do so as it's much more preferable to coordinate changes way beforehand. This is, practically, a one (two, sponge did most of the heavy lifting) man project.

Also beware, this project is community closed. Some minor background checks have to be done before u are fully accepted into the project. Feel free to host ur own server and play the game by urself or with ur friends, though. Join at ur sole discretion.

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
    stack haddock --haddock-docs
    ```
    
    This will produce two executables: `space-station15-client` and `space-station15-server`. Run them with the command:

    ```
    stack exec [executable name]
    ```

## macOS
macOS building works similarly to Linux, please refer to stack docs for further information.

## Linux
1. Install development libraries for SDL2 and OpenAL with your package manager.
2. To build, run:
    
    ```
    stack haddock --haddock-docs
    ```
    
    This will produce two executables: `space-station15-client` and `space-station15-server`. Run them with the command:

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
