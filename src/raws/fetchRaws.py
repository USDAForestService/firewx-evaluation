#! /usr/bin/python

import urllib2
import urllib
import numpy as np
import math
import os


##months = ['Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
locations = ['deermountain', 'tepee', 'westfork']

from xml.dom.minidom import parseString         #xml parser called minidom:



listspeed = []              #empty list for wind speed
listdirection = []              #empty list for winddirection
missingvalues = 0               #will give the amount of missing hourly data at the end
f = open('RAWSData.txt', 'w')
f.write('Location, Month, Day, Time, WindSpeed, WindDirection')
f.close()
f = open('RAWSData.txt', 'a')

for i in range(0, 3):
    print locations[i]
    geolocation = locations[i]
    if geolocation == 'deermountain':
        WIMS_ID = 242902
    elif geolocation == 'tepee':
        WIMS_ID = 352622
    elif geolocation == 'westfork':
        WIMS_ID = 242907
    for o in range(6, 12):
        month = months[o]
        for n in range(1, 32):               #for changing the day of the week in the web address below
            for m in range(0, 24):               #for iteratiing the hour of the day in the web address below
                try:
                        #web address to grab RAWS data with changing dates and hours
                        http = 'https://fam.nwcg.gov/wims/xsql/obs.xsql?stn=%s&sig=&user=&type=R&start=%s-%s-13&end=%s-%s-13&time=%s&user=' % (WIMS_ID, n, month, n, month, m)         #user can copy web address, replace '%s' with correct values for date and time, and veiw the data on a web browser  
                        #print http                     
                        response = urllib2.urlopen(http)                #for parsing
                        html = response.read()              #for parsing
                        response.close()                #for parsing
                        data = parseString(html)                #naming the parsed data
                      

                            #parsing for wind speed value only and converting to float, then appending it to the list for wind speed                   
                        xmlspeed = float((data.getElementsByTagName('wind_sp')[0].toxml()).replace('<wind_sp>','').replace('</wind_sp>',''))                    #.getElementByTagName defined:
                        listspeed.append(xmlspeed)                                                                                                              #lookes through xml page for the specified tag names (wind_sp)
                                                                                                                                                        #Grabs everything on that line starting at a specified point ([0])             
                                #parsing for wind direction value only and converting to float, then appending it to the list for wind direction        #then i replace the tag names with blank spaces so I am only left with a number
                        xmldirection = float((data.getElementsByTagName('wind_dir')[0].toxml()).replace('<wind_dir>','').replace('</wind_dir>',''))
                        listdirection.append(xmldirection)
                        
                        #u_comp = -(xmlspeed * math.cos(xmldirection))
                        #v_comp = -(xmlspeed * math.sin(xmldirection))

               
                except:
                        missingvalues = missingvalues + 1               #if hourly data is missing from web address, adds 1 to missingvalues and continues on through loop
                        continue
                f.write('\n')
                f.write(geolocation + ', ' + str(month) + ', ' + str(n) + ', ' + str(m) + ':00' + ', ' + str(xmlspeed) + ', ' + str(xmldirection))
                
f.close()
