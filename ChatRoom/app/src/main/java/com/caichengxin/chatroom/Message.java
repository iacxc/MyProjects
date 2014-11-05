package com.caichengxin.chatroom;

import java.util.Date;
import java.util.UUID;

/**
 * Created by caiche on 2014/11/2.
 */
public class Message
{
    private UUID mChatId;
    private String mText;
    private User mSender;
    private String mSenderImage;
    private Date mDate;

<<<<<<< HEAD
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
=======
    public Message(UUID chatId, User sender, String text) {
        mChatId = chatId;
        mSender = sender;
>>>>>>> 57285199702514457dcd98444512b32c4b8003e5
        mText = text;
        mDate = new Date();
    }

    public User getSender() {
        return mSender;
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

    public String getText() { return mText; }

    public String toString() {
        return mText + ", from " + mSender;
    }
}
