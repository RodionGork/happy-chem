# happy-chem

Just learning how to create haskell project

[Explanatory video](https://youtu.be/wMzyaH2-C1A)

1. Start the database, use check for host / creds in `Db.hs`
2. Build and start app (`stack build`, `stack exec happy-chem-exe`)
3. Navigate to UI at [http://localhost:18080](http://localhost:18080)
4. Alternatively use endpoints with curl like this:
    - `curl -X POST -d 'OO Hydrogen Peroxide' http://localhost:18080/molecule/784`
    - `curl http://localhost:18080/molecule/784` - to see this molecule
    - `curl http://localhost:18080/molecule` - to see all
    - and others, which could be learned from code
5. Reactions are added step by step. First add reaction ID and name, then
    its reagents, products and catalysts.
6. Verify by querying reaction by its ID (you'll see all ingredients etc)
7. Try `path_find` feature, entering ID's of two molecules.

### Settings

Few system parameters could be configured with env variables:

- `HTTP_PORT` - on which port web-server listens
- `DB_HOST` - database host to connect to
- `DB_USER` and `DB_PWD` - credentials to use with database

### Endpoints details

GET (returns data as text):

    /molecule - vew all molecules
    /molecule/<id> - vew specific molecule
    /catalyst - view all catalysts
    /catalyst/<id> - view specific catalyst
    /reaction - view list of reactions (ids and names)
    /reaction/<id> - view specific reaction with its reagents, products, catalysts etc.
    /find_path/<src_id>/<dst_id> - find path between two molecules with given ids

POST (accepts space-separated parameters in body besides ids in url)

    /molecule/<id> -d '<smiles> <name>' - add molecule (name can have spaces)
    /catalyst/<id> -d '<smiles> <name>' - add catalyst (name can have spaces)
    /reaction/<id> -d '<name>' - add reaction (unconnected yet)
    /reagent_in -d '<reaction_id> <molecule_id> <amount>' - add reagent for reaction
    /product_from -d '<molecule_id> <reaction_id> <amount>' - add product for reaction
    /accelerate -d '<reaction_id> <catalyst_id> <temperature> <pressure>' - add catalyst for reaction

Note that ID's for molecules and catalysts match their PubChem numbers.

### Possible improvements

1. Use JSON instead of TEXT on endpoints, obvious :)
2. Add some tests
3. Use Readable for entities
4. Streamline fetching path and reaction ingredients
5. Allow updating / deleting entities and links
6. I avoided using `LANGUAGE` pragma to see what "default" haskell looks like - it may be enabled to simplify code
