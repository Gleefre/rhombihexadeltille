LISP ?= sbcl

build:
	$(LISP) --load rhombihexadeltille.asd \
		--eval "(ql:quickload :deploy)" \
		--eval "(ql:quickload :rhombihexadeltille)" \
		--eval "(asdf:make :rhombihexadeltille)" \
		--eval "(quit)"
	cp NOTICE bin/
	cp RobotoMono-ExtraLight.ttf bin/
	mv bin/ rhombihexadeltille-game/
	zip rhombihexadeltille-game.zip rhombihexadeltille-game/*
	rm -r rhombihexadeltille-game/
