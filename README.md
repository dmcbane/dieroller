# Die Roller

Die Roller is a simple command line die roller for desktop RPG players. It provides precise control over die rolls via
command line parameters.

```
dieroller [ <option> ... ] [<arguments>] ...
  
  where the <arguments> are
  
    <dice>
  or
    <dice> <sides>
  or
    <dice> <sides> <modifier>
  or
    <dice> <sides> <modifier> <keep>
  
  See the --dice, --sides, and --modifier parameters for details.
  
  Examples:
  
    dieroller 5
    dieroller 1 10
    dieroller 3 6 +3
    dieroller 3 6 +6 2
    dieroller --dice 5 --sides 100 --modifier +4 --keep 3
    dieroller --dice 4 --sides 6 --keep 3
  
 where <option> is one of
  -v, --verbose : Display additional information (default to false).
  -d <dice>, --dice <dice> : Number of dice to roll.  Must be greater than 0.
    (default to 1)
  -k <keep>, --keep <keep> : Number of rolls to keep. Must be greater than 0 and less than or equal to <dice>.
    (default to number of dice)
  -m <modifier>, --modifier <modifier> : Modifier to the rolls. The first character can optionally
    be one of +, -, or * followed by a number.  If the +, -, or
    * are missing, + is assumed. (default to no modifier)
  -s <sides>, --sides <sides> : Number of sides per die. Must be greater than 0.
    (default to 20)
  -i <iterations>, --iterations <iterations> : Number of times to repeat the same rolls.  Must be greater than 0.
    (default to 1)
  --help, -h : Show this help
```

The repository also contains a Pathfinder character generator that will generate random characters using the classic,
standard, heroic, pool, and purchase methods.  The command line options below provide control over how characters are
generated.


```
pathfinder-character [ <option> ... ] [<arguments>] ...
  
  Examples:
  
    pathfinder-character --classic -v --number 10
    pathfinder-character -s -n 3
  
 where <option> is one of
/ -c, --classic : The classic method: 3D6 per ability.
| -s, --standard : The standard method: 4D6 keep high 3 per ability.
|   (this is the default)
| -r, --heroic : The heroic method: 2D6 plus 6 per ability.
| -l <diceperability>, --pool <diceperability> : The pool method: 24D6 for all 6 abilities. The parameter
|   specifies how many dice are assigned to each ability as
|   follows: 3/3/3/3/3/9 with a minimum of 3 dice per ability.
| -p <purchasetype>, --purchase <purchasetype> : The purchase method: parameters are set according to cost.
|   The parameter specifies the purchase type as follows: low,
|   standard, high, and epic fantasy which provides 10, 15, 20,
\   and 25 purchase points respectively.
  -v, --verbose : Display additional information (default to false).
  -n <n>, --number <n> : Number of characters to roll. Must be greater than 0.
    (default to 1)
  --help, -h : Show this help
```

# Building

Both dieroller and pathfinder-chacter are written in Racket for cross platform availability and ease of development.  (To
be more accurate, it's because I enjoy functional programming.)

From the command line, you can build either application with raco.

```
raco exe dieroller.rkt
```

or 

```
raco exe pathfinder-character.rkt
```
