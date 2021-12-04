#include <RcppArmadillo.h>
#include <boost/progress.hpp>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <string>


// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

std::map< std::string, std::list<double>> prod_curve (
    const arma::uvec & well_id,
    const arma::vec & prod_first_6,
    const arma::vec & prod_first_12,
    const arma::vec & prod_first_24,
    const arma::vec & prod_first_60,
    const arma::vec & prod_last_year,
    const arma::vec & prod_daily_last_year,
    const arma::vec & prod_cumulative,
    const arma::vec & prod_peak_daily,
    const arma::vec & date_start,
    const arma::vec & date_end,
    const arma::vec & peak_month,
    const arma::vec & decline_3,
    const arma::vec & decline_12,
    const arma::vec & decline_24,
    const arma::vec & decline_60,
    double step_threshold
    )

{

    // final result map
    std::map< std::string, std::list<double>> output;

    // number of wells
    int n_wells = prod_cumulative.size();

    // loop over individual wells
    boost::progress_display show_progress(n_wells);

    for(int i = 0; i < n_wells; i++) {

        //----------------------------------------------------------------------------------------------
        // make cumulative production curve for well

        arma::vec cumulative_production;

        cumulative_production
            << 0                                                // assume production starts at 0
            << prod_first_6[i]                                  // months 0-6  cumulative production
            << prod_first_12[i]                                 // months 0-12 cumulative production
            << prod_first_24[i]                                 // months 0-24 cumulative production
            << prod_first_60[i]                                 // months 0-60 cumulative production
            << prod_cumulative[i] - prod_last_year[i]           // cumulative production year before report data
            << prod_cumulative[i]                               // cumulative production to date
            << arma::endr;

        // dates for for cumulative production entries

        // calculate number of months well has been active
        double months_active = std::round( (date_end[i] - date_start[i]) * 12 );

        arma::vec dates_cumulative_production;

        dates_cumulative_production
            << 0                                    // start
            << 6                                    // month 6
            << 12                                   // month 12
            << 24                                   // month 24
            << 60                                   // month 60
            << months_active - 12                   // year before end month
            << months_active                        // end month
            << arma::endr;


        // remove missing data
        arma::uvec id = arma::find_finite( cumulative_production );

        dates_cumulative_production = dates_cumulative_production(id);
        cumulative_production = cumulative_production(id);

        // keep only dates less than or equal to months active
        id = arma::find( dates_cumulative_production <= months_active );

        dates_cumulative_production = dates_cumulative_production(id);
        cumulative_production = cumulative_production(id);

        // sort cumulative production curve in ascending date order
        id = arma::sort_index( dates_cumulative_production );

        dates_cumulative_production = dates_cumulative_production(id);
        cumulative_production = cumulative_production(id);

        // remove descending production
        arma::vec prod_diff = arma::diff(cumulative_production);
        id = arma::find( prod_diff < 0 ) + 1;

        dates_cumulative_production.shed_rows(id);
        cumulative_production.shed_rows(id);


        //----------------------------------------------------------------------------------------------
        // make monthly production curve

        // peak production per month
        double prod_peak_monthly = prod_peak_daily[i] * 30;

        arma::vec monthly_production;

        monthly_production
            << 0                                                // assume production starts at 0
            << prod_first_6[i] / 6                              // months 0 - 6 average monthly production
            << (prod_first_12[i] - prod_first_6[i] ) / 6        // months 6 -12 average monthly production
            << (prod_first_24[i] - prod_first_12[i]) / 12       // months 12-24 average monthly production
            << (prod_first_60[i] - prod_first_24[i]) / 36       // months 24-60 average monthly production
            << prod_peak_monthly                                // production during peak month
            << prod_peak_monthly * (1 - decline_3[i] / 100 )    // production 3  months after peak
            << prod_peak_monthly * (1 - decline_12[i] / 100)    // production 12 months after peak
            << prod_peak_monthly * (1 - decline_24[i] / 100)    // production 24 months after peak
            << prod_peak_monthly * (1 - decline_60[i] / 100)    // production 60 months after peak
            << prod_daily_last_year[i] * 30                     // average monthly production during last year of operation
            << arma::endr;

        // dates for monthly production entries
        arma::vec dates_monthly_production;

        dates_monthly_production
            << 0                        // start month
            << 6                        // month 6
            << 12                       // month 12
            << 24                       // month 24
            << 60                       // month 60
            << peak_month[i]            // month of peak production
            << peak_month[i] + 3        // 3  months after peak production
            << peak_month[i] + 12       // 12 months after peak production
            << peak_month[i] + 24       // 24 months after peak production
            << peak_month[i] + 60       // 60 months after peak production
            << months_active            // last month of production
            << arma::endr;

        // remove missing data
        id = arma::find_finite( monthly_production );

        dates_monthly_production = dates_monthly_production(id);
        monthly_production = monthly_production(id);


        // keep only dates less than or equal to months active
        id = arma::find( dates_monthly_production <= months_active );

        dates_monthly_production = dates_monthly_production(id);
        monthly_production = monthly_production(id);

        // sort monthly production curve in ascending date order
        id = arma::sort_index( dates_monthly_production );

        dates_monthly_production = dates_monthly_production(id);
        monthly_production = monthly_production(id);


        // remove duplicate months
        // take duplicate month with biggest production value
        std::vector<int> dup_remove;

        for(int j = 1; j < dates_monthly_production.size(); j++){

            // test if adjacent dates are the same
            if( dates_monthly_production[j] == dates_monthly_production[j-1] ){

                // test which date has greater production value
                if( monthly_production[j] >= monthly_production[j-1] ) {
                    dup_remove.push_back(j);
                } else {
                    dup_remove.push_back(j-1);
                }
            }
        }


        if( dup_remove.size() >=1 ){

            arma::uvec dup_remove_arma = arma::conv_to< arma::uvec >::from(dup_remove);

            dates_monthly_production.shed_rows(dup_remove_arma);
            monthly_production.shed_rows(dup_remove_arma);
        }


        //----------------------------------------------------------------------------------------------
        // interpolate monthly production (linear interpolation)
        // ignore wells with only one production datapoint

        if( dates_monthly_production.size() > 1 ){

            // interpolation dates
            arma::vec dates_interpolate = arma::regspace(0, 0.1, months_active);

            // interpolate with fast linear (assumes monotonic increasing x)
            arma::vec monthly_production_interpolate;

            arma::interp1(dates_monthly_production,
                          monthly_production,
                          dates_interpolate,
                          monthly_production_interpolate,
                          "*linear");



            //----------------------------------------------------------------------------------------------
            // make normalized production curve
            // interpolated cumulative production so that it equals
            // empirical cumulative production

            // loop over cumulative production vector
            int n = cumulative_production.size();                               // number of empirical data points for cumulative production
            double cumulative_counter = 0;                                      // cumulative production counter

            arma::vec prod_cum_norm(monthly_production_interpolate.size());     // normalized cumulative production
            arma::vec prod_monthly_norm(monthly_production_interpolate.size()); // normalized monthly production

            for(int j = 1; j < n; j++){

                // get empirical production total during current time window
                double prod_current = cumulative_production[j] - cumulative_production[j-1];

                // check for negative production
                if( prod_current < 0 ){
                    prod_current = 0;
                }

                // check for cumulative production overshoot
                if( cumulative_counter >= prod_cumulative[i] ){
                    prod_current = 0;
                }


                // get interpolated production during current time window
                id = arma::find(   dates_interpolate >= dates_cumulative_production[j-1]
                                && dates_interpolate <= dates_cumulative_production[j] );


                arma::vec prod_interp_sub = monthly_production_interpolate(id);

                // sum of interpolated production
                double interp_sum = arma::sum(prod_interp_sub);

                // normalized production
                if( interp_sum == 0){
                    // if interpolated production sums to 0, set normalized cumulative production
                    // to sum of empirical current production + empirical cumulative production

                    arma::vec tmp(id.size());
                    tmp.fill(cumulative_counter + prod_current);
                    prod_cum_norm(id) = tmp;


                } else {
                    // if interpolated production is non-zero, set cumulative production
                    // to cumulative sum  of interpolated production, normalized to
                    // be equal to empirical current production

                    prod_cum_norm(id) = arma::cumsum( prod_interp_sub ) / interp_sum * prod_current + cumulative_counter;

                }

                // advance cumulative production counter
                cumulative_counter = cumulative_counter + prod_current;
            }


            // test for step function
            // get first differences of cumulative production
            // test if any value greater than step_threshold x cumulative production

            arma::vec prod_diff = arma::diff(prod_cum_norm);

            if(arma::max(prod_diff) >= step_threshold*prod_cumulative[i]){

                arma::interp1(dates_cumulative_production,
                              cumulative_production,
                              dates_interpolate,
                              prod_cum_norm,
                              "*linear");
            }


            // append well id, dates, and production estimate to output list
            // append if whole month only
            for(int t = 0; t < dates_interpolate.size(); t++){

                // if whole month, pushback to output
                if( dates_interpolate[t] == round(dates_interpolate[t]) ){

                    output["well_id"].push_back(well_id[i]);
                    output["production_month"].push_back( dates_interpolate[t] );
                    output["cumulative_production"].push_back( prod_cum_norm[t] );

                }
            }

        } // end interpolation


        // advance progress bar
        ++show_progress;

    }

    return output;

}
