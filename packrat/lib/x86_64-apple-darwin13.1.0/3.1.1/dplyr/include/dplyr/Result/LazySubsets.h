#ifndef dplyr_LazySubsets_H
#define dplyr_LazySubsets_H

namespace dplyr {
     
    class LazySubsets {
    public:
        typedef dplyr_hash_map<SEXP,SEXP> DataMap ;
        typedef DataMap::const_iterator const_iterator ;
        
        LazySubsets(){}
        
        LazySubsets( const DataFrame& df) : data_map(){
            CharacterVector names = df.names() ;
            for( int i=0; i<df.size(); i++){
                data_map[as_symbol(names[i])] = df[i] ;    
            }
        }
        virtual ~LazySubsets(){}
        
        virtual SEXP get_variable(SEXP symbol) const {
            DataMap::const_iterator it = data_map.find(symbol) ;
            return it->second ;
        }
        virtual bool is_summary( SEXP symbol ) const {
            return false ;    
        }
        virtual int count(SEXP symbol) const{
            return data_map.count(symbol);    
        }
        
        virtual void input( SEXP symbol, SEXP x){
            data_map[symbol] = x;    
        }
        
        inline const_iterator find(SEXP x) const {
            return data_map.find(x) ;
        }
        
        inline const_iterator end() const {
            return data_map.end() ;   
        }
        
        virtual int size() const{ 
            return data_map.size() ; 
        }
        
        inline SEXP& operator[](SEXP symbol){
            return data_map[symbol] ;    
        }
        
        inline int nrows() const {
            return Rf_length( data_map.begin()->second ) ;  
        }
        
    private:
        DataMap data_map ;
    } ;

}

#endif
