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
    printf("      [--tz timezone] [--interval interval]\n");
    printf("\n");
    exit(1);
}

int main(int argc, char *argv[])
{
    int i;
    int j;
    int k;

    int year;
    int month;
    int day;
    int hour;
    int minute;
    int second;
    double lat;
    double lon;
    int tz;
    int interval;

    if(argc < 10){
        Usage();
    }
    
    i = 1;
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
            tz = atoi(argv[++i]);
        }
        else if(EQUAL(argv[i], "--interval"))
        {
            interval = atoi(argv[++i]);
        }
        else
        {
            Usage();
        }
        i++;
    }

    struct posdata pd, *pdat; /* declare a posdata struct and a pointer for
                                 it (if desired, the structure could be
                                 allocated dynamically with malloc) */
    long retval;              /* to capture S_solpos return codes */


    pdat = &pd; /* point to the structure for convenience */

    /* Initialize structure to default values. (Optional only if ALL input
       parameters are initialized in the calling code, which they are not
       in this example.) */

    S_init (pdat);

    pdat->longitude = lon;   /* DECIMAL DEGREES */
    pdat->latitude  =  lat;  /* DECIMAL DEGREES */
    pdat->timezone  =  tz;   /* DO NOT ADJUST FOR DAYLIGHT SAVINGS TIME. */

    pdat->daynum    =  203;    /* the algorithm will compensate for leap year */

    pdat->hour      = hour;
    pdat->minute    = minute;
    pdat->second    = second;

    /* Let's assume that the temperature is 27 degrees C and that
       the pressure is 1006 millibars.  The temperature is used for the
       atmospheric refraction correction, and the pressure is used for the
       refraction correction and the pressure-corrected airmass. */

    pdat->temp      =   27.0;
    pdat->press     = 1006.0;

    /* assume a flat surface facing southeast,
       tilted at latitude. */

    pdat->tilt      = pdat->latitude;  /* Tilted at latitude */
    pdat->aspect    = 135.0;       /* 135 deg. = SE */

    return 0;
}
