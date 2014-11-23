package com.caichengxin.count24;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by caiche on 2014/11/2.
 */
public class Calculate {
    private List<String> mAnswerList=new ArrayList<String>();

    public List<String> getAnswerList() {
        return mAnswerList;
    }

    public static class Data{
        public String expStr = "";
        public float[]  arr;
        public String[] strs;
        
        public Data(){}

        public Data(int v1, int v2, int v3, int v4) {
            arr = new float[]{v1, v2, v3, v4};
            strs= new String[]{String.valueOf(v1),
                                   String.valueOf(v2),
                                   String.valueOf(v3),
                                   String.valueOf(v4)};
        }
    }

    public void count(Data data){
        float[] arr = data.arr;
        if(arr.length <= 1){
            if(arr.length == 1 && arr[0] == 24){
                mAnswerList.add(data.expStr.substring(1));
            }
            return ;
        }
        for(int i=0; i<arr.length-1; i++){
            for(int j=i+1; j<arr.length; j++){
                float x=arr[i];
                float y=arr[j];
                String xs=data.strs[i];
                String ys=data.strs[j];
                for(int k=0; k<6; k++){
                    Data newData = getNewArr(data,i);
                    switch(k){
                        case 0:
                            newData.arr[j-1] = x + y;
                            newData.expStr   = xs + "+" + ys;
                            break;
                        case 1:
                            newData.arr[j-1] = x - y;
                            newData.expStr   = xs + "-" + ys;
                            break;
                        case 2:
                            newData.arr[j-1] = y - x;
                            newData.expStr   = ys + "-" + xs;
                            break;
                        case 3:
                            newData.arr[j-1] = x * y;
                            newData.expStr   = xs + "*" + ys;
                            break;
                        case 4:
                            if(y!=0){
                                newData.arr[j-1] = x / y;
                                newData.expStr   = xs + "/" + ys;
                            }else {
                                continue;
                            }
                            break;
                        case 5:
                            if(x!=0){
                                newData.arr[j-1] = y / x;
                                newData.expStr   = ys + "/" + xs;
                            }else {
                                continue;
                            }
                            break;
                    }
                    
                    newData.expStr = "(" + newData.expStr + ")";
                    newData.strs[j-1] = newData.expStr;
                    count(newData);
                }
            }
        }

    }
    
    private static Data getNewArr(Data data, int i) {
        //get a copy of data, except the ith
        //keep the expStr no change
        Data newData= new Data();
        newData.expStr = data.expStr;
        newData.arr=new float[data.arr.length-1];
        newData.strs=new String[data.arr.length-1];
        
        for(int m=0,len=data.arr.length,n=0;m<len;m++){
            if(m!=i){
                newData.arr[n]  = data.arr[m];
                newData.strs[n] = data.strs[m];
                n++;
            }
        }
        return newData;
    }

    public static List<String> easyCount(int[] values)
    {
        Calculate cal=new Calculate();
        cal.count(new Data(values[0], values[1], values[2], values[3]));
        Set<String> set=new HashSet<String>(cal.getAnswerList());
        return new ArrayList<String>(set);
    }

}
