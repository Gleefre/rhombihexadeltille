LISP ?= sbcl

build:
	$(LISP) --load rhombihexadeltille.asd \
		--eval "(ql:quickload :deploy)" \
		--eval "(ql:quickload :sketch)" \
		--eval "(push :deploy *features*)" \
		--eval "(asdf:load-system :rhombihexadeltille :force t)" \
		--eval "(asdf:make :rhombihexadeltille)" \
		--eval "(quit)"
	cp NOTICE bin/
	cp RobotoMono-Bold.ttf bin/
	rm -rf rhombihexadeltille-game/
	mv bin/ rhombihexadeltille-game/
	zip rhombihexadeltille-game.zip rhombihexadeltille-game/*
	rm -rf rhombihexadeltille-game/
