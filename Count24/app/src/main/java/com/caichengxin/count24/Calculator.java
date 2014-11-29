package com.caichengxin.count24;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;

/**
 * Created by caiche on 2014/11/2.
 */
public class Calculator {
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

    public void count24(Data data, List<String> answerList){
        float[] arr = data.arr;
        if(arr.length <= 1){
            if(arr.length == 1 && arr[0] == 24){
                answerList.add(data.expStr.substring(1));
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
                    count24(newData, answerList);
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

    public static List<String> easyCount(int i1, int i2, int i3, int i4)
    {
        Calculator cal=new Calculator();
        List<String> answerList=new ArrayList<String>();

        cal.count24(new Data(i1, i2, i3, i4), answerList);
        Set<String> set=new HashSet<String>(answerList);
        return new ArrayList<String>(set);
    }

    public static boolean isNumeric(String str){
        for (int i = 0; i < str.length(); i++){
            if (!Character.isDigit(str.charAt(i)))
                return false;
        }
        return true;
    }

    public static int getPri(String s) throws Exception
    {
        switch (s.charAt(0)) {
            case '+':
            case '-':
                return 0;
            case '*':
            case '/':
                return 1;
            case '(':
            case ')':
                return -1;
            default:
                throw new Exception("Invalid character: " + s.charAt(0));
        }
    }

    public static void check(String s, Stack<String> opStack, Stack<String> suffixExpr)
            throws Exception
    {
        if (opStack.size() == 0) {
            opStack.push(s);
            return;
        }

        if (s.equals("(") || s.equals(")")) {
            if (s.equals("("))
                opStack.push(s);
            else {
                while (! opStack.peek().equals("("))
                    suffixExpr.push(opStack.pop());

                opStack.pop();
            }
        }
        else {
            if (getPri(s) <= getPri(opStack.peek())){
                suffixExpr.push(opStack.pop());
                check(s, opStack, suffixExpr);
            }
            else
                opStack.push(s);
        }
    }


    public static Stack<String> infixToSuffix(Vector<String> infixExpr)
            throws Exception
    {
        Stack<String> opStack = new Stack<String> ();
        Stack<String> suffixExpr = new Stack<String> ();
        for (String s : infixExpr) {
            if (isNumeric(s))
                suffixExpr.push(s);
            else
                check(s, opStack, suffixExpr);
        }

        for (String s : opStack)
            suffixExpr.push(s);

        return suffixExpr;
    }

    public static int calculate(Vector<String> infixExpr)
            throws Exception
    {
        Stack<String> stackResult = new Stack<String>();

        for (String s : infixToSuffix(infixExpr)) {
            if (isNumeric(s))
                stackResult.push(s);
            else {
                int op1 = Integer.valueOf(stackResult.pop());
                int op2 = Integer.valueOf(stackResult.pop());
                int res;

                switch (s.charAt(0)) {
                    case '+': res = op2 + op1; break;
                    case '-': res = op2 - op1; break;
                    case '*': res = op2 * op1; break;
                    default : res = op2 / op1; break;
                }
                stackResult.push(String.valueOf(res));
            }
        }
        return Integer.valueOf(stackResult.get(0));
    }

}