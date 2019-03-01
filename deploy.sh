#!/bin/sh

cd /var/www/pilotpower.table1.org
git pull origin master
docker-compose -f docker-compose.prod.yml build

CONTAINER=$(docker ps -q --filter="NAME=pilotpower")

docker cp $CONTAINER:/usr/local/lib/R/site-library/shiny/www/shared/ /var/www/pilotpower.table1.org/app/www/
docker cp $CONTAINER:/opt/shiny-server/assets/ /var/www/pilotpower.table1.org/app/www/__assets__/
docker cp $CONTAINER:/opt/shiny-server/node_modules/shiny-server-client/dist/shiny-server-client.min.js /var/www/pilotpower.table1.org/app/www/__assets__/

docker-compose -f docker-compose.prod.yml up -d