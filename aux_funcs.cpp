//[ [Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <set>

using namespace Rcpp;


bool between(int x, int y, int a, int b) {
  return (a <= x && x < b) || (y <= b && y > a);
}

// [[Rcpp::export]]
IntegerMatrix Cpp_add_missing_intervals(IntegerMatrix original_data, int first_point=0){
  //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  // Assume data sorted by id!
  //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  int cols = original_data.ncol();
  
  IntegerMatrix new_matrix(original_data.nrow()*2, 3);
  
  for(int i = 0; i < new_matrix.length(); i++) {
    new_matrix[i] = NA_INTEGER;
  }
  int new_row_iter = 0;
  
  int current_id = original_data(0, 0);
  //int next_id;
  std::vector<int> tps; //(NA_INTEGER);
  tps.push_back(first_point);
  
  for(int orig_iter = 0; orig_iter < original_data.nrow(); orig_iter++) {
    
    current_id = original_data(orig_iter,0);
    
    for(int c = 1; c < cols; c++) { // don't add ids, so start c=1
      int val = original_data(orig_iter, c);
      if(val != NA_INTEGER)
        tps.push_back(val);
    }
    
    //next_id = original_data(orig_iter+1, 0);
    if(orig_iter == original_data.nrow() -1 || current_id != original_data(orig_iter+1, 0)) {
      // Sort timepoints
      std::sort(tps.begin(), tps.end());
      tps.resize(std::distance(tps.begin(), std::unique(tps.begin(), tps.end())));
      std::vector<int>::iterator tpprev = tps.begin();
      
      // Make rows for each timepoint
      std::vector<int>::iterator tpiter = tpprev;
      tpiter++;
      for(; tpiter != tps.end(); tpiter++) {
        if(new_row_iter >= new_matrix.nrow()) {
          IntegerMatrix new_new_matrix(new_matrix.nrow()*2, 3);
          
          for(int i = 0; i < new_matrix.nrow(); i++) {
            for(int j = 0; j < new_matrix.ncol(); j++) {
              new_new_matrix(i,j) = new_matrix(i,j);
            }
          }
          for(int i = new_matrix.nrow(); i < new_new_matrix.nrow(); i++) {
            for(int j = 0; j < new_new_matrix.ncol(); j++) {
              new_new_matrix(i,j) = NA_INTEGER;
            }
          }
          
          new_matrix = new_new_matrix;
        }
        new_matrix(new_row_iter, 0) = current_id;
        new_matrix(new_row_iter, 1) = *(tpprev);
        new_matrix(new_row_iter, 2) = *(tpiter);
        
        new_row_iter++;
        
        tpprev = tpiter;
      }
      // Clear timepoints for next id
      tps.clear();
      tps.push_back(first_point);
    }
    
  }
  return(new_matrix);
}
// [[Rcpp::export]]
IntegerMatrix Cpp_add_event_indicators(IntegerMatrix orig_data, IntegerMatrix event_data, bool permanent=false) {
  // Expect orig_data AND event_data to be sorted by ids in first column
  int current_id = orig_data(0,0);
  int event_id_iter = 0;
  bool event_happened = false;
  //int orig_id_iter = 0;
  IntegerMatrix ind_vect(orig_data.nrow(), event_data.ncol()-1); // initialized to 0 in all slots
  for(int orig_iter = 0; orig_iter < orig_data.nrow(); orig_iter++) {
    if(current_id != orig_data(orig_iter,0)) {
      current_id = orig_data(orig_iter,0);
      //orig_id_iter = orig_iter;
      //continue;
    }
    for(int event_iter = event_id_iter; event_iter < event_data.nrow(); event_iter++) {
    //Rprintf(" %d %d %d\n", current_id, orig_data(orig_iter, 0), event_data(event_iter,0));
      if(current_id != event_data(event_iter,0)) {
        if(current_id > event_data(event_iter,0)) {
          event_id_iter = event_iter+1;
          event_happened = false;
          continue;
        }
        else 
          break;
      }
      
      for(int k = 1; k < event_data.ncol(); k++) {
        if((permanent && event_happened) || orig_data(orig_iter, 2) == event_data(event_iter,k)) {
          event_happened = true;
          ind_vect(orig_iter, k-1) = 1;
        }
      }
    }
  }
  return(ind_vect);
}

// [[Rcpp::export]]
NumericVector Cpp_add_med_col(NumericMatrix data, NumericMatrix meddata) {
  NumericVector result(data.nrow());
  //LogicalVector d(data.nrow());
  for(int j = 0; j < data.nrow(); j++) {
    for(int i = 0; i < meddata.nrow(); i++) {
      if(data(j,0) == meddata(i,0) && data(j,1) >= meddata(i,1) && data(j,2) <= meddata(i,2)) {
        result(j) = meddata(i,4);
      }
    }
  }
  return(result);
}