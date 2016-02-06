/******************************************************************************
 *
 * $Id$
 *
 * Project:  Solpos interface  
 * Purpose:  Compute solar radiation with solpos
 * Author:   Natalie Wagenbrenner <nwagenbrenner@gmail.com> 
 *
 ******************************************************************************
 *
 * THIS SOFTWARE WAS DEVELOPED AT THE ROCKY MOUNTAIN RESEARCH STATION (RMRS)
 * MISSOULA FIRE SCIENCES LABORATORY BY EMPLOYEES OF THE FEDERAL GOVERNMENT 
 * IN THE COURSE OF THEIR OFFICIAL DUTIES. PURSUANT TO TITLE 17 SECTION 105 
 * OF THE UNITED STATES CODE, THIS SOFTWARE IS NOT SUBJECT TO COPYRIGHT 
 * PROTECTION AND IS IN THE PUBLIC DOMAIN. RMRS MISSOULA FIRE SCIENCES 
 * LABORATORY ASSUMES NO RESPONSIBILITY WHATSOEVER FOR ITS USE BY OTHER 
 * PARTIES,  AND MAKES NO GUARANTEES, EXPRESSED OR IMPLIED, ABOUT ITS QUALITY, 
 * RELIABILITY, OR ANY OTHER CHARACTERISTIC.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 *****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "solpos00.h"
#include <iostream>
using namespace std;

#define EQUAL(a,b) (strcmp(a,b)==0)

void Usage()
{
    printf("Usage:\n");
    printf("compute_solar [--year year] [--month month]\n");
    printf("      [--day day] [--hour hour]\n");
    printf("      [--minute minute] [--second second]\n");
    printf("      [--lat latitude] [--lon longitude]\n");
    printf("      [--tz timezone] [--verbose]\n");
    printf("\n");
    printf("Returns:\n");
    printf("extraterrestrial global horizontal solar irradiance [W/m^2]\n");
    printf("\n");
    printf("Example:\n");
    printf("compute_solar --year 1999 --month 7 --day 22 --hour 9 ");
    printf("--minute 45 --second 37 --lat 33.65 --lon -84.43 --tz -5.0\n");
    printf("\n");
    exit(1);
}

int main(int argc, char *argv[])
{
    int year = -1;
    int month = -1;
    int day = -1;
    int hour = -1;
    int minute = -1;
    int second = -1;
    double lat = -1.0;
    double lon = -1.0;
    double tz = -9999.0;
    bool verbose = false;

    if(argc < 19){
        Usage();
    }
    
    int i = 1;
    while(i < argc)
    {
        if(EQUAL(argv[i], "--year"))
        {
            year = atoi(argv[++i]);
        }
        else if(EQUAL(argv[i], "--month"))
        {
            month = atoi(argv[++i]);
        }
        else if(EQUAL(argv[i], "--day"))
        {
            day = atoi(argv[++i]);
        }
        else if(EQUAL(argv[i], "--hour"))
        {
           hour  = atoi(argv[++i]);
        }
        else if(EQUAL(argv[i], "--minute"))
        {
            minute = atoi(argv[++i]);
        }
        else if(EQUAL(argv[i], "--second"))
        {
            second = atoi(argv[++i]);
        }
        else if(EQUAL(argv[i], "--lat"))
        {
            lat = atof(argv[++i]);
        }
        else if(EQUAL(argv[i], "--lon"))
        {
            lon = atof(argv[++i]);
        }
        else if(EQUAL(argv[i], "--tz"))
        {
            tz = atof(argv[++i]);
        }
        else if(EQUAL(argv[i], "--verbose"))
        {
            verbose = true;
        }
        else
        {
            Usage();
        }
        i++;
    }

    if(verbose){
        cout<<" "<<endl;
        cout<<"year = "<<year<<endl;
        cout<<"month = "<<month<<endl;
        cout<<"day = "<<day<<endl;
        cout<<"hour = "<<hour<<endl;
        cout<<"minute = "<<minute<<endl;
        cout<<"second = "<<second<<endl;
        cout<<"lat = "<<lat<<endl;
        cout<<"lon = "<<lon<<endl;
        cout<<"tz = "<<tz<<endl;
    }

    struct posdata pd, *pdat; /* declare a posdata struct and a pointer for it */

    long retval;              /* to capture S_solpos return codes */

    pdat = &pd; /* point to the structure for convenience */

    /* Initialize structure to default values. (Optional only if ALL input
       parameters are initialized in the calling code, which they are not
       in this example.) */

    S_init (pdat);

    pdat->longitude = lon;   /* DECIMAL DEGREES */
    pdat->latitude  = lat;  /* DECIMAL DEGREES */
    pdat->timezone  = tz;   /* DO NOT ADJUST FOR DAYLIGHT SAVINGS TIME. */

    pdat->year      = year;
//    pdat->daynum    =  203;    /* the algorithm will compensate for leap year */
    pdat->function = (~S_DOY); /* disable DOY bit */  
    pdat->month     = month;
    pdat->day       = day;
    
    pdat->hour      = hour;
    pdat->minute    = minute;
    pdat->second    = second;

    /* temperature is used for atmospheric refraction correction, pressure is used for 
       refraction correction and the pressure-corrected airmass. */

//    pdat->temp      =   27.0;
//    pdat->press     = 1006.0;

    /* optionally assume a flat tilted surface */
    /* don't use this since RAWS measurement is normal to flat groud */

//    pdat->tilt      = pdat->latitude;  /* Tilted at latitude */
//    pdat->aspect    = 135.0;       /* 135 deg. = SE */

    retval = S_solpos (pdat);  /* S_solpos function call */
    S_decode(retval, pdat);    /* look at the return code! */

    if(verbose){
        cout<<"return code = "<<retval<<endl;
    }

    cout<<pdat->etr<<endl; //global horizontal solar irradiance

    return 0;
}
