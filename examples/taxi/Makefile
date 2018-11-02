run: nodes
	docker-compose up --build

nodes: node1/node1.hs node2/node2.hs node1/Taxi.hs node2/Taxi.hs

node1/Taxi.hs: Taxi.hs
	cp Taxi.hs node1

node2/Taxi.hs: Taxi.hs
	cp Taxi.hs node2

node1/node1.hs node2/node2.hs: generate
	./generate

generate: generate.hs
	ghc generate -i../../src

clean:
	rm -f generate generate.hi generate.o node1/node1.hs node2/node2.hs \
	    node1/Taxi.hs node2/Taxi.hs

.PHONY: clean nodes
