#!/bin/sh

cd /var/www/pilotpower.table1.org
git pull origin master
docker-compose -f docker-compose.prod.yml build
docker-compose -f docker-compose.prod.yml up -d