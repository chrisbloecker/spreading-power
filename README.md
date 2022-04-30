# spreading-power

A small program to calculate the spreading power of nodes according to an SIR disease spreading model.

It's easiest to compile this program with [stack](https://docs.haskellstack.org/en/stable/README/) by running `stack build` (or `stack install --local-bin-path .` to compile it and place the binary in the current directory, if you want).

The program is used like so:
```
Usage: spreading-power-exe --input INPUT --output OUTPUT [--directed]
                           --seed SEED --iterations ITERATIONS
  spreading-power

Available options:
  -h,--help                Show this help text
  --input INPUT            Path to input file
  --output OUTPUT          Path to output file
  --directed               Whether the graph is directed
  --seed SEED              Random number seed
  --iterations ITERATIONS  Number of iterations to run per node
```

For example `../spreading-power-exe --input network.edgelist --output network.spreading_power --seed 42 --iterations 1000`. The input file should contain an edgelist, such as
```
1 2
1 2
1 4
2 3
2 4
3 4
4 5
5 6
5 7
5 8
```
where each line corresponds to one link from the first node to the second. By default, the network is interpreted as undirected, that is, if there is an entry `u v`, then the corresponding link can be traversed in either direction.
