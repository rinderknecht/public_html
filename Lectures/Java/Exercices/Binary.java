/*
 * Binary.java
 *
 * Created on 12 novembre 2003, 00:58
 */

/**
 *
 * @author  DARTHGAN
 */
public class Binary{
    
    /** Creates a new instance of Binary */
    public Binary() {
    }
    
    static String intBin(int nb){
      StringBuffer _sbuf = new StringBuffer();
      
      if(nb==0)
          System.out.println("0");
      else{
          while((nb/2)>=1){
              _sbuf.append(nb%2);
              nb/= 2;
          }
          _sbuf.append(nb);
      }
      _sbuf.reverse();
      return _sbuf.toString();
    }
    
    public static void main(String [] args){
        int nb= Integer.parseInt(args[0]);
        System.out.println();
        System.out.println(args[0]+" est "+intBin(nb)+" en binaire ");
  
    }
}
