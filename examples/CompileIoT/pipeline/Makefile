run: node1/node1.hs node2/node2.hs node3/node3.hs
	docker-compose up --build

node1/node1.hs node2/node2.hs node3/node3.hs: generate
	./generate

generate: generate.hs
	ghc generate

clean:
	rm -f generate generate.hi generate.o node1/node1.hs node2/node2.hs node3/node3.hs

.PHONY: clean
