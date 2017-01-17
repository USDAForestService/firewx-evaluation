
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
#include <stdlib.h>
#include <string.h>
#include "deadfuelmoisture.h"
#include <iostream>
#include <stdio.h>
using namespace std;

#define EQUAL(a,b) (strcmp(a,b)==0)

void Usage()
{
    printf("Usage:\n");
    printf("compute_fms [--input_file inFile] [--output_file outFile] [--verbose]\n");
    printf("\n");
    printf("Returns:\n");
    printf("1-hr, 10-hr, 100-hr, and 1000-hr fuel moistures are written to fms_output.txt.\n");
    printf("\n");
    printf("Example:\n");
    printf("compute_fms --input_file fms_input.txt --output_file fms_output.txt\n");
    printf("\n");
    printf("Each row of the input file must have the following space-delimited variables.\n");
    printf("The variables must be in the following order with no header.\n");
    printf("Note that the first line contains three additional variables.\n");
    printf("\n");
    printf("startYear               [4 digits]\n");
    printf("startMonth              [1 = Jan, 12 = Dec]\n");
    printf("startDay                [1-31]\n");
    printf("startHour               [0-23]\n");
    printf("startMinute             [0-59]\n");
    printf("startSecond             [0-59]\n");
    printf("startMillisecond        [0-999]\n");
    printf("startAirTemp            [C]\n");
    printf("startAirHumidity        [g/g]\n");
    printf("startSolarRad           [W/m2]\n");
    printf("startCumRain            [cm]\n");
    printf("stickTemp               [C] (*first line only)\n");
    printf("stickSurfHumidity       [g/g] (*first line only)\n");
    printf("stickMoisture           [g/g] (*first line only)\n");
    printf("\n");
    exit(1);
}

int main(int argc, char *argv[])
{
    const char *inputFile;
    const char *outputFile;
    bool verbose = false;

    if(argc < 5){
        Usage();
    }
    
    int i = 1;
    while(i < argc)
    {
        if(EQUAL(argv[i], "--input_file")) 
        {
            inputFile = argv[++i];
        }
        else if(EQUAL(argv[i], "--output_file")) 
        {
            outputFile = argv[++i];
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

    double fuelMoisture_1;          // volume-weighted mean moisture content (g/g)
    double fuelMoisture_10;
    double fuelMoisture_100;
    double fuelMoisture_1000;

    //create the sticks
    Sem::DeadFuelMoisture* dfm1h = Sem::DeadFuelMoisture::createDeadFuelMoisture1("stick_1hr");
    Sem::DeadFuelMoisture* dfm10h = Sem::DeadFuelMoisture::createDeadFuelMoisture10("stick_10hr");
    Sem::DeadFuelMoisture* dfm100h = Sem::DeadFuelMoisture::createDeadFuelMoisture100("stick_100hr");
    Sem::DeadFuelMoisture* dfm1000h = Sem::DeadFuelMoisture::createDeadFuelMoisture1000("stick_1000hr");

    //initialize the sticks
    dfm1h->initializeStick();
    dfm10h->initializeStick();
    dfm100h->initializeStick();
    dfm1000h->initializeStick();

    FILE *inFile;
    FILE *outFile;

    inFile = fopen(inputFile, "r");
    if(inFile == 0){
        cout<<"Can't open input file '"<<inputFile<<"'."<<endl;
        exit(1);
    }

    outFile = fopen(outputFile, "a");

    fprintf(outFile, "year,month,day,hour,1hrfm,10hrfm,100hrfm,1000hrfm\n");

    //read first line for stick initialization
    fscanf(inFile, "%d %d %d %d %d %d %d %lf %lf %lf %lf %lf %lf %lf", &startYear, &startMonth,
            &startDay, &startHour, &startMinute, &startSecond, &startMillisecond, &startAirTemp,
            &startAirHumidity, &startSolarRad, &startCumRain,
            &stickTemp, &stickSurfHumidity, &stickMoisture);

    if(verbose){
        cout<<"startYear = "<<startYear<<endl;
        cout<<"startMonth = "<<startMonth<<endl;
        cout<<"startDay = "<<startDay<<endl;
        cout<<"startHour = "<<startHour<<endl;
        cout<<"startMinute = "<<startMinute<<endl;
        cout<<"startSecond = "<<startSecond<<endl;
        cout<<"startMillisecond = "<<startMillisecond<<endl;
        cout<<"startAirTemp = "<<startAirTemp<<endl;
        cout<<"startAirHumidity = "<<startAirHumidity<<endl;
        cout<<"startSolarRad = "<<startSolarRad<<endl;
        cout<<"startCumRain = "<<startCumRain<<endl;
        cout<<"stickTemp = "<<stickTemp<<endl;
        cout<<"stickSurfHumidity = "<<stickSurfHumidity<<endl;
        cout<<"stickMoisture = "<<stickMoisture<<endl;
    }
    
    //initialize the stick environment
    dfm1h->initializeEnvironment(startYear, startMonth, startDay, startHour, startMinute, startSecond,
            startAirTemp, startAirHumidity, startSolarRad, startCumRain, stickTemp,
            stickSurfHumidity, stickMoisture);

    dfm10h->initializeEnvironment(startYear, startMonth, startDay, startHour, startMinute, startSecond,
            startAirTemp, startAirHumidity, startSolarRad, startCumRain, stickTemp,
            stickSurfHumidity, stickMoisture);

    dfm100h->initializeEnvironment(startYear, startMonth, startDay, startHour, startMinute, startSecond,
            startAirTemp, startAirHumidity, startSolarRad, startCumRain, stickTemp,
            stickSurfHumidity, stickMoisture);

    dfm1000h->initializeEnvironment(startYear, startMonth, startDay, startHour, startMinute, startSecond,
            startAirTemp, startAirHumidity, startSolarRad, startCumRain, stickTemp,
            stickSurfHumidity, stickMoisture);

    while((fscanf(inFile, "%d %d %d %d %d %d %d %lf %lf %lf %lf",
                    &endYear, &endMonth, &endDay, &endHour, &endMinute,
                   &endSecond, &endMillisecond, &endAirTemp, &endAirHumidity,
                  &endSolarRad, &endCumRain)) != EOF){ 

        //update the sticks
        dfm1h->update(endYear, endMonth, endDay, endHour, endMinute, endSecond,
                 endAirTemp, endAirHumidity, endSolarRad, endCumRain);

        dfm10h->update(endYear, endMonth, endDay, endHour, endMinute, endSecond,
                 endAirTemp, endAirHumidity, endSolarRad, endCumRain);

        dfm100h->update(endYear, endMonth, endDay, endHour, endMinute, endSecond,
                 endAirTemp, endAirHumidity, endSolarRad, endCumRain);

        dfm1000h->update(endYear, endMonth, endDay, endHour, endMinute, endSecond,
                 endAirTemp, endAirHumidity, endSolarRad, endCumRain);

        //get the stick moisture contents
        fuelMoisture_1 = dfm1h->meanWtdMoisture();
        fuelMoisture_10 = dfm10h->meanWtdMoisture();
        fuelMoisture_100 = dfm100h->meanWtdMoisture();
        fuelMoisture_1000 = dfm1000h->meanWtdMoisture();

        if(verbose){
            cout<<"1-hr fuel moisture = "<<fuelMoisture_1<<endl;
            cout<<"10-hr fuel moisture = "<<fuelMoisture_10<<endl;
            cout<<"100-hr fuel moisture = "<<fuelMoisture_100<<endl;
            cout<<"1000-hr fuel moisture = "<<fuelMoisture_1000<<endl;
        }
        
        fprintf(outFile, "%d,%d,%d,%d,%lf,%lf,%lf,%lf\n", endYear, endMonth, endDay,
                endHour, fuelMoisture_1, fuelMoisture_10, fuelMoisture_100, fuelMoisture_1000);
    }

    //clean up
    fclose(inFile);
    fclose(outFile);

    return 0;
}
