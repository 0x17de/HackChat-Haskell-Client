FROM haskell

RUN apt-get update
RUN apt-get install ncurses-dev libncursesw5-dev
RUN cabal update
RUN cabal install c2hs
RUN cabal install websockets mtl aeson concurrent-extra hscurses threads

