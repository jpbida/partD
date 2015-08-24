#!/bin/bash

## If the results exist delete them ##
hadoop fs -rm -r /user/vagrant/sup_drg*

### This command is used to submit the map reduce job ###

hadoop jar /opt/cloudera/parcels/CDH-4.7.0-1.cdh4.7.0.p0.40/lib/hadoop-mapreduce/hadoop-streaming.jar \
-files  /vagrant/rpgms/medical/mapper_loc_spec_drg.R,/vagrant/rpgms/medical/sup_all_red.R  -mapper mapper_loc_spec_drg.R -reducer sup_all_red.R \
-input /user/vagrant/partd_data.tab -output /user/vagrant/sup_drg
