Read @AGENTS.md.

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads) for issue tracking. Use `~/go/bin/bd` commands instead of markdown TODOs. See AGENTS.md for workflow details.

# Development

1. The `.docs/` directory is your resource for all knowledge
2. If you can't find knowledge in the docs directory, find it online and download it there
3. The `.state/` directory is your resource for any file you need to manage when you do your work, as well as for information from other agents that you're not allowed to manage but are allowed to browse.


## Testing
```bash
# Run all tests (standard - minimal output)
~/bin/eldev test -B

# Run all tests for a specific version of emacs
~/bin/eldev docker 30 test

# Run all tests with backtraces (debugging)
~/bin/eldev test

# Run tests with full debugging (CI command)
~/bin/eldev -p -dtT test

# Run specific test file
~/bin/eldev test -B --file="test/organizing-test.el"

# Stop on first failure
~/bin/eldev test -B --stop

# Test specific functions (buttercup selectors)
~/bin/eldev test -B "clarify"
```

### Running some debug/test code in the correct environment
```bash
~/bin/eldev exec -f debug-code.el
```

# Process

First, read CLAUDE.md.
Then, read doc/technique-for-drawing-directed-graphs.asciidoc.

## Implementation Principles

### Junction characters for ASCII graphs

In order to enhance the visual aesthetic of the ASCII graphs, we implement a simple algorithm that walks the edge in order to determine the locally-relevant algorithm (starting port, ending port, middle of the edge) that will tell us the correct semigraphic to use where relevant.

Junction characters exist at specific points along the edges:

#### At the start of the edge, at the port boundary.

Example of a missing junction character at the starting port:

	   ┌──────────┐
	   │Research  │
	   └─────│────┘
			 │

This should be:

	   ┌──────────┐
	   │Research  │
	   └─────┬────┘
			 │

Equivalent logic would be applied if the port for edge start were to be on the right side of the node, where instead of `─` you would have `├` (to signify both the continuation of the node visual boundary vertically and the horizontal start of the edge).

#### When the edge requires a direction change

Example of an edge going towards the right and requiring a downward corner at the end of the edge right before the destination port:

				│
				└──────────────────────│
							  ┌────────▼────────┐

This should be:

				└──────────────────────┐
							  ┌────────▼────────┐


#### When two edges join, or two edges separate
Here is an example of two edges joining without a junction character. Here we have two edges merging on a horizontal line: two edges coming from the top that join in this leftward line (the downward corner at the left goes toward at least one more node)

								   │                  │
			  ┌───────────────────────────────────────┘

This could also be broken if it looked this way:

								   │                  │
			  ┌────────────────────│──────────────────┘

It should instead look this way:

								   │                  │
			  ┌────────────────────┴──────────────────┘


#### When two edges cross

This is very simple, if two edges cross, instead of the character for whichever edge was drawn last, we should have `┼` drawn instead.

#### Applications of the rules
These rules apply in any situation where we would draw an edge character. This means the one possible exemption is where an arrow is placed (which may be the last element of the edge, depending on implementation).

Here is an example of a missing t-junction where an edge goes toward an arrow (destination port) and another edge splits off towards the right:

				│
				└─────────────(more edge this way...)
	┌───────────▼──────────┐


this should be drawn as:

				│
				├─────────────(more edge this way...)
	┌───────────▼──────────┐

Because there is an edge going down to the downward arrow as well.
