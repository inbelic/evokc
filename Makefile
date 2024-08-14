NAME=Evokc

GHC=ghc
GHCI=ghci

ODIR=../build

_PACKAGES=base containers parsec
PACKAGES=$(patsubst %,$\-package %,$(_PACKAGES))

build:
	cd $(NAME); \
	$(GHC) --make -o $(ODIR)/$(NAME) Main -odir $(ODIR) \
	-hidir $(ODIR) $(PACKAGES)

run: build
	./build/$(NAME)

clean:
	rm -rf build/*

.PHONY: build clean
