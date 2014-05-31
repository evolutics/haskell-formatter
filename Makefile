SOURCE = src
MAIN = Main.hs
NAME = HaskellFormatter
TRASH = ~/.local/share/Trash/files

all:
	hlint $(SOURCE)
	cd $(SOURCE); ghc --make $(MAIN) -o ../$(NAME) -fforce-recomp

clean:
	find $(SOURCE) \( -name "*.hi" -o -name "*.o" \) -execdir mv -b {} \
		$(TRASH) \;
