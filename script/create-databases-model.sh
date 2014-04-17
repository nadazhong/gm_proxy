#!/bin/sh

echo "deleting database slg_gm_proxy, if it exists..."
mysql -uroot -p123456 -e "drop database slg_gm_proxy" 2> /dev/null 
echo "creating database slg_gm_proxy ....."
mysql -uroot -p123456 -e "create database slg_gm_proxy" || exit 1
echo "creating slg_gm_proxy necessary tables...."   
mysql -uroot -p123456 slg_gm_proxy --force  < ./tables.sql || exit 1
