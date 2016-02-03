/******************************************************************************
 *
 * $Id$
 *
 * Project:  Fire wx evaluation  
 * Purpose:  Compute fuel moisture with Nelson model
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

#include "newfms.h"
#include <iostream>
using namespace std;

int main()
{
    int startYear;                  // (4 digits)
    int startMonth;                 // (1 = Jan, 12 = Dec)
    int startDay;                   // (1-31)
    int startHour;                  // (0-23)
    int startMinute;                // (0-59) 
    int startSecond;                // (0-59)
    int startMillisecond;           // (0-999)
    double startAirTemp;            // (C)
    double startAirHumidity;        // (g/g)
    double startSolarRad;           // (W/m2)
    double startCumRain;            // (cm)
    double stickTemp;               // (C)
    double stickSurfHumidity;       // (g/g)
    double stickMoisture;           // (g/g)

    int endYear;                  // (4 digits)
    int endMonth;                 // (1 = Jan, 12 = Dec)
    int endDay;                   // (1-31)
    int endHour;                  // (0-23)
    int endMinute;                // (0-59) 
    int endSecond;                // (0-59)
    int endMillisecond;           // (0-999)
    double endAirTemp;            // (C)
    double endAirHumidity;        // (g/g)
    double endSolarRad;           // (W/m2)
    double endCumRain;            // (cm)

    Fms *pStick1hr;
    char *stick1hr; 

    //create the 1-hr stick
    pStick1hr = Fms_Create1Hour(stick1hr);

    cout<<"pStick1hr->a = stick radius = "<<pStick1hr->a<<endl;

    //testing
    startYear = 2015;
    startMonth = 7;
    startDay = 3;
    startHour = 13;
    startMinute = 0;
    startSecond = 0;
    startMillisecond = 0;
    startAirTemp = 30;
    startAirHumidity = 0.0029;
    startSolarRad = 800;
    startCumRain = 0;
    stickTemp = 25;
    stickSurfHumidity = 0.0031;
    stickMoisture = 0.0038;
    
    endYear = 2015;
    endMonth = 7;
    endDay = 3;
    endHour = 18;
    endMinute = 0;
    endSecond = 0;
    endMillisecond = 0;
    endAirTemp = 33;
    endAirHumidity = 0.015;
    endSolarRad = 800;
    endCumRain = 1.5;
    //end testing

    //initialize the 1-hr stick
    Fms_InitializeAt(pStick1hr, startYear, startMonth, startDay, startHour, startMinute, startSecond,
        startMillisecond, startAirTemp, startAirHumidity, startSolarRad, startCumRain, stickTemp,
        stickSurfHumidity, stickMoisture);
                    
    //update the 1-hr stick
    Fms_UpdateAt(pStick1hr, endYear, endMonth, endDay, endHour, endMinute, endSecond,
            endMillisecond, endAirTemp, endAirHumidity, endSolarRad, endCumRain);

    cout<<"1-hr stick equilibirum moisture content = "<<pStick1hr->sem<<endl;
    cout<<"max stick moisture content = "<<pStick1hr->wmax<<endl;

    //clean up
    Fms_Destroy(pStick1hr);

    return 0;
}
