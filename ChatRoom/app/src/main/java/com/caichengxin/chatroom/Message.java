package com.caichengxin.chatroom;

/**
 * Created by caiche on 2014/11/2.
 */
public class Message
{
    private String mOwner;
    private String mMessage;

    public Message(String owner, String message)
    {
        mOwner = owner;
        mMessage = message;
    }

    public String getOwner() {
        return mOwner;
    }

    public String getMessage() {
        return mMessage;
    }

    public String toString() { return mMessage + " from " + mOwner;}
}
