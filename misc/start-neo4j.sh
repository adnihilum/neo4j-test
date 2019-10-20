#/bin/bash
docker run \
    --publish=7474:7474 --publish=7687:7687 \
    --volume=$HOME/neo4j/data:/data \
    -v $HOME/neo4j/plugins:/plugins \
    --env NEO4J_dbms_security_procedures_unrestricted=algo.* \
    neo4j
    #:3.4.14
