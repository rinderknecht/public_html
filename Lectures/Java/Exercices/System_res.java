/*
 * System_res.java
 *
 * Created on 6 novembre 2003, 00:17
 */

/**
 *
 * @author  DARTHGAN
 */
import java.lang.*;
import java.text.NumberFormat;

public class System_res {
    
    /** Creates a new instance of System_res */
    public System_res() {
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        
        double a= Double.parseDouble(args[0]);
        double b= Double.parseDouble(args[1]);
        double c= Double.parseDouble(args[2]);
        double d= Double.parseDouble(args[3]);
        double e= Double.parseDouble(args[4]);
        double f= Double.parseDouble(args[5]);
        int    k= 0;
        double detA=  (a*e)-(b*d);
        double detB1= (c*e)-(b*f);
        double detB2= (a*f)-(d*c);
        double x=0 , y = 0;
        NumberFormat numFormat = NumberFormat.getNumberInstance();
        numFormat.setMaximumFractionDigits(2);
        numFormat.setMinimumFractionDigits(2);
        
        if(
detA !=0){
            x= detB1/detA;
            y= detB2/detA;
            k= 1;
        }
        else{
            if((detB1!=0)||(detB2!=0))
                k= 2;
            if((detB1==0)&&(detB2==0))
                k= 3;
        }
            
        switch(k){
            case 1: System.out.println("Les solution su systeme sont : ");
                    System.out.println(" x = "+ numFormat.format(x));
                    System.out.println(" y = "+ numFormat.format(y));
                    break;
            
            case 2: System.out.println(" Le systeme n'a pas de solutions");
                    break;
                    
            case 3: System.out.println(" Le systeme presente une infinite de solutions");
            
         
                
            default:System.out.println("");
                    break;
        }
    }
    
}
