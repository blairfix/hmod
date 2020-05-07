#include <Rcpp.h>
#include <iostream>
#include <vector>
#include <string>
#include <boost/algorithm/string/replace.hpp>


using namespace Rcpp;


// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

std::vector<std::string> string_replace (   std::vector<std::string> original,
                        					std::vector<std::string> replacement,
                        					std::vector<std::string> input_string
                    )
{

    // input string size
    int n_input = input_string.size();

    // output string
    std::vector<std::string> return_string(n_input);

    // replacement string size
    int n_replace = replacement.size();
    int n_original = original.size();

    if( n_original != n_replace){

        std::cout << "Original and replacement vectors not the same size!";

    } else{

        #pragma omp parallel for firstprivate(input_string, original, replacement)
        for(int input_iterator = 0; input_iterator < n_input; input_iterator++){

            std::string new_string = input_string[input_iterator];


            // loop over replacement string
            for(int replace_iterator = 0; replace_iterator < n_original; replace_iterator++){

                std::string temp_original = original[replace_iterator];
                std::string temp_replace = replacement[replace_iterator];

                // replace in place
                boost::replace_all(new_string, temp_original, temp_replace);

            }

            return_string[input_iterator] = new_string;
        }

    }


return(return_string);

}

