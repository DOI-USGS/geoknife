## `geoknife` mock test files.  

### webdata  
test_webdata_fabric.rds - A single timestep prism fabric.  
test_webdata_prism_year.rds - A one year prism webdata.  

### webprocess  
test_webprocess_knife.rds - A default webprocess / knife object.  
test_webprocess_knife_prod.rds - A default webprocess / knife object from prod.  
test_webprocess_algorithms.rds - The algorithm list.  
test_webprocess_algorithms.rds - The algorithm list from prod.  
test_webprocess_sleep-plus-five - A web process algorithm plus five seconds sleep.
test_webprocess_tab.rds - A web process set to TAB as the delimiter.
test_XML_wp_opendapsubset.rds - A webprocessing opject for OPeNDAP subset.  

### webgeom  
test_webgeom_huc08.rds - A webgeom made from a single huc08.  
test_simplgeom_two_points.rds - A simplegeom with two points.

### XML templating related  
test_XML_no_gmlid_wd.rds - a webdata object from custom processing vignette.  
test_XML_no_gmlid_wg.rds - a sciencebase web geom with no subsetting applied.  

test_XML_no_gmlid_xml.xml - An xml output file to check against for no gmlid filter.  
test_XML_sg_xml.xml - An xml output file to check against for simple geometry.  
test_XML_two_points_xml.xml - An XML output file to check two points simple geometry.  
test_XML_wg_xml.xml - Basic execute XML for testing.  

test_email.xml - XML file for testing email when finished execute request.  
test_email_gj.rds - geojob grabbed while in a browser().  
test_email_wp.rds - webprocess grabbed while in a browser().  

test_getgridtimerange.xml - XML file for testing getgridtimerange  
test_listopendapgrids.xml - XML file for testing listopendapgrids  
test_wfsgetfeature.xml - XML file for testing wfsgetfeature templating  

### query
test_query_webdata.rds - A query to sciencebase output.