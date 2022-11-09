LISP ?= sbcl

build:
	$(LISP) --eval "(ql:quickload :deploy)" \
		--eval "(ql:quickload :sketch)" \
		--eval "(push :deploy *features*)" \
		--load rhombihexadeltille.asd \
		--eval "(asdf:load-system :rhombihexadeltille :force t)" \
		--eval "(asdf:make :rhombihexadeltille)" \
		--eval "(quit)"
	cp NOTICE bin/
	cp RobotoMono-Bold.ttf bin/
	cp soundtrack.wav bin/
	rm -rf rhombihexadeltille-game/
	mv bin/ rhombihexadeltille-game/

clean:
	rm -rf rhombihexadeltille-game/
	rm -rf rhombihexadeltille-game.zip
