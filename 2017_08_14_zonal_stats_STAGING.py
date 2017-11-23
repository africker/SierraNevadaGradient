"""
Purpose to run the zonal stats to table tool for multiple rasters in a directory
then merge into a single table for analysis and join to original s

D17 zonal statistics calculator
@author: MHP,
modified by GAF
name: 2017_01_17_zonal stats_STAGING.py
date: 2017_01_17
purpose: to compute the statistics for different grid cells for multiple rasters
Input: a folder with rasters and a zone polygon 
"""

import arcpy, os #math
from datetime import datetime
start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
arcpy.CheckOutExtension("Highways")
arcpy.env.overwriteOutput=True

#input and output parameters
##set the workspace address
scale = "500"
base_root = r"D:\Google_Drive\RESEARCH_ACADEMIC\ASU\GRADIENT\SOIL_CONTINUOUS_VARS\DBF_out\%sm" %scale
env.workspace = base_root

ZS_Out = arcpy.CreateFolder_management (base_root, "ZS_Out")                                 #output folder
outFolder = base_root + os.sep + "ZS_Out" 

##set the zone feature class
zones= r'I:\ASU_LAB\04_GENERAL_NEON_DATA\E_COPY\D17_GIS\PAPER_1_ANALYSIS\ZS_STAGING\inVector_Soil_Geology\D17_%sm_v2.shp' % scale

##set the zone field
zoneField="TARGET_FID"

rasterDir = r"D:\Google_Drive\RESEARCH_ACADEMIC\ASU\GRADIENT\SOIL_CONTINUOUS_VARS\CONVERTED_RASTER"
env.workspace = rasterDir
rasters = arcpy.ListRasters()
print "# of input rasters = " + str(len(rasters))
statList = ["MEAN"]#,"STD","MAXIMUM"]

for stat in statList:
    print stat + " = statistic being calculated"
    for inValueRaster in rasters:
        print inValueRaster + "= inValueRaster"
        rName = inValueRaster[:-4] + "_%s" % stat
        print rName + " = rName"
        outputTable = outFolder + os.sep + rName
        print outputTable    
        noDataOption = "NODATA" 
        zonalSummaryType = stat #"MEAN" ##set statistics type: MEAN, SUM, MAX, MIN. OR ALL
        splitNumber = 6 ##Set splite number. if you have a million feature, 10-15 would be good.
        oidfield= 'FID'
        rows=arcpy.SearchCursor(zones)
        countFeature=0
        for row in rows:
            countFeature+=1
        print 'Number of features in the zone dataset is: ', countFeature
        divisionNumber = int((countFeature/splitNumber)+1)
        partsCount=(int((countFeature/divisionNumber)+1))
        print 'each part has ' + str(divisionNumber) + ' features'
        tableList=[]
        print tableList
        for i in range(0,partsCount):
            arcpy.MakeFeatureLayer_management (zones, "zonelyr")
            selected=arcpy.SelectLayerByAttribute_management ("zonelyr", "NEW_SELECTION", '"FID" >=' +str(divisionNumber*i) + 'AND "FID" <' + str((i+1)*divisionNumber))
            #print 'selection is done for part ' + str (i+1)
            Output= arcpy.CreateFeatureclass_management(env.workspace, "selected"+str(i)+".shp", "POLYGON", zones)
            #print 'the layer for part ' + str(i+1)+' is created'
            arcpy.CopyFeatures_management(selected,Output)
            print 'selected features of part '+str(i+1)+' are being copied...'
            try:
                outZSaT = ZonalStatisticsAsTable("selected"+str(i)+".shp", zoneField, inValueRaster,"tblPart"+str(i), noDataOption, zonalSummaryType)
                tablePart='tblPart'+str(i)
                tableList.append(env.workspace + os.sep + tablePart)
                print 'zonal analysis for part ' +str(i+1) +' is done'
            except:
                print 'I got an error; skiping this part'
            arcpy.Delete_management("selected" +str(i) +".shp")
        print tableList
        print outputTable
        arcpy.Merge_management(tableList, outputTable)
        print 'tables are merged'
        for i in range(0,len(tableList)):
            try:    
                arcpy.Delete_management("tblPart" +str(i))
            except:
                pass
        print "Table Created for Raster: " + os.sep + inValueRaster
       
print "joining tables"
env.workspace = outFolder
tList = arcpy.ListTables()
print str(tList) + " = tables in outfolder"
masterTableGDBname = "masterTableGDB"
masterTableGDB = arcpy.CreateFileGDB_management (outFolder, masterTableGDBname , "CURRENT")
print str(masterTableGDB) + "= masterTableGDB"
arcpy.TableToGeodatabase_conversion (tList, masterTableGDB)
env.workspace = outFolder + os.sep + "masterTableGDB.gdb"

tListMean = arcpy.ListTables("*MEAN")
tListStd = arcpy.ListTables("*STD")
tListMax = arcpy.ListTables("*MAXIMUM")

tbl = tListMean[0]    
masterTableGDB = str(masterTableGDB) + os.sep + "masterTableGDB"
arcpy.Copy_management(tbl,masterTableGDB)
    
for t in tListMean:
    varName = t #[:-4] trying without removing extension.
    varNameMean = varName[:3]+"_MEAN"
    #print varName + " = varName"
    arcpy.JoinField_management(masterTableGDB, "TARGET_FID", t , "TARGET_FID")
    arcpy.AddField_management(masterTableGDB, varNameMean, "FLOAT", "20", "4", "", varNameMean, "NULLABLE", "NON_REQUIRED", "")  # Process: Add Field
    arcpy.CalculateField_management(masterTableGDB, varNameMean, "[MEAN]", "VB", "")  # Process: Calculate Field
    arcpy.DeleteField_management(masterTableGDB, "MEAN")     # Process: Delete Field
    arcpy.DeleteField_management(masterTableGDB, ["TARGET_FID_1", "COUNT_1", "AREA_1"])
    print  " joined MEAN table to masterTableGDB"
    
for t in tListStd:
    varName = t #[:-4] trying without removing extension.
    varNameStd = varName[:3]+"_SD"
    #print varName + " = varName"
    arcpy.JoinField_management(masterTableGDB, "TARGET_FID", t , "TARGET_FID")
    arcpy.AddField_management(masterTableGDB, varNameStd, "FLOAT", "20", "4", "", varNameStd, "NULLABLE", "NON_REQUIRED", "")
    arcpy.CalculateField_management(masterTableGDB, varNameStd, "[STD]", "VB", "")  # Process: Calculate Field
    arcpy.DeleteField_management(masterTableGDB, "STD")     # Process: Delete Field
    arcpy.DeleteField_management(masterTableGDB, ["TARGET_FID_1", "COUNT_1", "AREA_1"])
    print  " joined STD table to masterTableGDB"

for t in tListMax:
    varName = t #[:-4] trying without removing extension.
    varNameMax = varName[:3]+"_MAX"
    #print varName + " = varName"
    arcpy.JoinField_management(masterTableGDB, "TARGET_FID", t , "TARGET_FID")
    arcpy.AddField_management(masterTableGDB, varNameMax, "FLOAT", "20", "4", "", varNameMax, "NULLABLE", "NON_REQUIRED", "")
    arcpy.CalculateField_management(masterTableGDB, varNameMax, "[MAX]", "VB", "")  # Process: Calculate Field
    arcpy.DeleteField_management(masterTableGDB, "MAX")     # Process: Delete Field
    arcpy.DeleteField_management(masterTableGDB, ["TARGET_FID_1", "COUNT_1", "AREA_1"])
    print  " joined MAXIMUM table to masterTableGDB"
    
Output_Geodatabase = outFolder + os.sep + "masterTableGDB.gdb"
arcpy.FeatureClassToGeodatabase_conversion (zones, Output_Geodatabase)

fcList = arcpy.ListFeatureClasses()
for fc in fcList:
    arcpy.JoinField_management(fc,"TARGET_FID", masterTableGDB , "TARGET_FID")
    print "Joined Master Table to Feature Class = " + fc
    arcpy.DeleteField_management(fc, ["Join_Count", "TARGET_FID_1", "AREA", "MEAN_1"])
    # Process: Convert Table To CSV File
    #csvName = outFolder + os.sep + fc +"_csv"
    #arcpy.ConvertTableToCsvFile_roads(fc, csvName, "COMMA")
   
print 'All zonal statistics tables joined in: ' + outFolder   
finish = datetime.now() 
finish_str=str(finish)
print"program finish date, time = %s" % finish_str
totaltime= finish-start
print 'total processing time = %s' % totaltime