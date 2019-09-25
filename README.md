# ie-trapgen

This command-line tool generates WeiDU scripts which will add traps (regions) into *.ARE files.

The areas affected, traps amount, geometry and effects are read from a json input file.

Various randomization options are supported.

## Usage

```
> trapgen.exe <input file> <ouput file>
```

The input file must exist. 
If the output file exists, it will be overwritten, otherwise it will be created.

## Input

Input file is stored in json format. An example is provided in this repository (see data.json).

The following value types are used, when describing the JSON structure below:

- STRING - json string enclosed in double quotes ("some string")
- ARRAY  - json array enclosed in square brackets. Elements are separated by commas. (array of strings - \[ "aaa", "bbb", "ccc"\])
- OBJECT - json object enclosed in curly braces ({...}). All objects have a defined structure.
- INTEGER - precise (no randomization) integral number, enclosed in double quotes ("25")
- RANDOM  - random integral number, enclosed in double quotes. See description below
- POINT   - pair of RANDOM values, separated by comma ("200,400-450")


Most integer values support randomization (unless the value type is INTEGER) and can be written in 3 forms:

1. Strict number. This works as a normal precise integer ("detect": "30"). No randomization is applied.
2. Range. Two integers (min and max) separated by '-' ("detect": "20-40"). A random number from the specified range is used.
3. Delta. Two ingerers (base and delta) separated by '\~' ("disarm": "60\~10"). A random number is generated in a range from (base - delta) to (base + delta). 60~10 represents a range from 50 to 70.

### Input file structure

See data.json for reference


**/(JSON root object):**
```
{
  "random_effects": [], // ARRAY: list of random effects 'flavors'. Used in randomization of trap effects
  "areas" : []          // ARRAY: list of areas to modify
}
```

/random_effects/\[**effects group**\]
```
{
  "flavor" : "common",  // STRING: name of effects group (for example "undead", "mages", "kobolds")
  "tiers" : []          // ARRAY: list of effect 'tiers' (flavor subgroups)
}
```

/random_effects/\[flavor\]/\[**tier**\]
```
{ 
  "id": "arrows",   // STRING: name of this tier
  "scripts" : []    // ARRAY: list of strings, which are names of trap BCS scripts (for example ["GTAR","GTAR5"])
}
```

/areas/\[**area**\]
```
{
  "id": "AR2602",   // STRING: area to modify
  "groups": []      // ARRAY: list of trap groups
}
```

/areas/\[area\]/\[**trap group**\]
```
{
  "id": "bridge",   // STRING: name of this group
  "pick":"1-3",     // RANDOM: amount of traps to pick from the "traps" array
  "traps": []       // ARRAY: list of all possible traps in this group
}
```

/areas/\[area\]/\[trap group\]/\[**trap**\]
```
{
  "id":"arrow1",    // STRING: name of this trap
  "detect":"20~5",  // RANDOM: trap detection difficulty
  "disarm":"30-35", // RANDOM: trap disarm difficulty
  "effect": { },    // OBJECT: trap effect object 
  "geometry": { }   // OBJECT: trap geometry object
}
```

/areas/\[area\]/\[trap group\]/\[trap\]/**effect**

Fixed effect (no randomization, doesn't use effects data from **/random_effects/** )
```
{
  "type":"fixed", // must be "fixed" for fixed effect
  "script":"GTAR" // name of trap script to use
}
```

Random effect. Randomly picks one effect from the specified flavor/tier combination under **/random_effects/** structure
```
{
  "type":"random",      // must be "random" for random effect
  "flavor_id":"common", // STRING: id of one of effect groups from /random_effects/*
  "tier_id":"arrows"    // STRING: id of one of tiers from /random_effects/[flavor]/*
}
```

/areas/\[area\]/\[trap group\]/\[trap\]/**geometry**

Points geometry. Basically an array of POINTs (pairs of RANDOM values, see above).
```
{
    "type": "points", // must be "points" for this object type
    "points": [
        "70,100",
        "170,90-110",
        "170~10,100",
        "70~10,90-110"
    ]
}
```


Rectangle geometry. Defined by width, height, rotation angle, and center. 

A rectangle with defined width and height is created with center at\[0,0], rotated clockwise by the specified angle (in degrees), and offset to the specified new center point.
```
{
  "type": "rectangle",    // must be "rectangle" for this object type
  "width": "100-110",     // RANDOM: rectangle width before rotation is applied
  "height": "60",         // RANDOM: rectangle height before rotation is applied
  "angle": "45",          // RANDOM: rotation angle (degrees, clockwise) before offset to the new center
  "center": "300~20,415~20"   // POINT: final center point of the rectangle
}
```


## Output

Output file is a WeiDU script, that can be directly copy-pasted into your mod tp2 file, or automatically generated and used during mod installation (see **AT_NOW** command in WeiDU documentation). 

This generated script expects a certain patch function (**add_are_trap**) to be available during execution. A reference function is provided in this repository (see include.tph.example).
