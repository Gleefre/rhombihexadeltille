LISP ?= sbcl

build:
	$(LISP) --load rhombihexadeltille.asd \
		--eval "(ql:quickload :deploy)" \
		--eval "(push :deploy *features*)" \
		--eval "(ql:quickload :rhombihexadeltille)" \
		--eval "(asdf:make :rhombihexadeltille)" \
		--eval "(quit)"
	cp NOTICE bin/
	cp RobotoMono-ExtraLight.ttf bin/
	rm -rf rhombihexadeltille-game/
	mv bin/ rhombihexadeltille-game/
	zip rhombihexadeltille-game.zip rhombihexadeltille-game/*
	rm -rf rhombihexadeltille-game/
