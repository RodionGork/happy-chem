# happy-chem

Just learning how to create haskell project

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

Note that ID's for molecules and catalysts match their PubChem numbers.
