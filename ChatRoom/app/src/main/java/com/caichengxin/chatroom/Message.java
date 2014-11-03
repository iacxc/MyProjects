package com.caichengxin.chatroom;

import java.util.Date;
import java.util.UUID;

/**
 * Created by caiche on 2014/11/2.
 */
public class Message
{
    private UUID mId;
    private String mText;
    private Person mSender;
    private String mSenderImage;
    private Date mDate;

    public Message() {
        mId = UUID.randomUUID();
        mDate = new Date();
    }

    public UUID getId() {
        return mId;
    }


    public String getText() {
        return mText;
    }

    public void setText(String text) {
        mText = text;
    }

    public Person getSender() {
        return mSender;
    }

    public void setSender(Person sender) {
        mSender = sender;
    }

    public String getSenderImage() {
        return mSenderImage;
    }

    public void setSenderImage(String senderImage) {
        mSenderImage = senderImage;
    }

    public Date getDate() {
        return mDate;
    }

    public void setDate(Date date) {
        mDate = date;
    }

    public String toString() {
        return mText;
    }
}
