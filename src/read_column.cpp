#include <Rcpp.h>
#include <fstream>
#include <string>
#include <vector>


using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

std::vector<std::string> read_column(   std::string filename,
                                        int col_number,
                                        std::string delimiter)
{
    // 1. open input file
    std::ifstream input_file(filename, std::ifstream::in);
    if (!input_file.is_open())
    {
        throw std::range_error("File does not exist");
    }

    std::cout << "Reading " << filename << " ..." << std::endl;

    // 2.  read in file
    int line_number = 1;
    std::string line;
    std::list<std::string> output;

    while (getline(input_file, line))
    {

        std::string data;
        size_t pos;

        // if first col, get data
        if(col_number == 1) {

            pos = line.find(delimiter);
            data = line.substr(0, pos);

        } else {

            // loop over columns and erase delimiter
            for(int i = 1; i < col_number; i++){

                // erase tab
                pos = line.find(delimiter);
                line.erase(0, pos + delimiter.length());

            }

            //get data
            data = line.substr(0, line.find(delimiter) );

        }


        output.push_back(data);

        // output line number
        int test = round( line_number / 5000000) * 5000000 ;
        if (line_number == test) {
            std::cout << "reading line " << line_number << std::endl;
        }

        line_number++;
    }


    // 3. convert output to vector and return
    std::vector<std::string> result(output.begin(), output.end());
    return result;

}
