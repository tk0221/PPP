#include <stdio.h>

int main (int argc, char **argv) {

//      c4 = table (
//          c1 : T F,
//          c2 : F *,
//          c3 : T T );
// 
// forwards to
// 
//    (c1 && ! c2 && c3) || ( ! c1 && 1 && c3 )


    int x = 1, y = 2, z = 3 ;

    int c = 0 ;

    c = ({ int temp_c1 = x > 4 ;
           int temp_c2 = y < x ;
           int temp_c3 = z == 3 || z > y ;
           (temp_c1 && ! temp_c2 && temp_c3) || ( ! temp_c1 && 1 && temp_c3 ) ; }) ;

    printf ("c = %d\n", c ) ;

    return 0 ;
}
