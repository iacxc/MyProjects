package com.caichengxin.chatroom;

import java.util.Date;
import java.util.UUID;


public class Message
{
    private final UUID mChatId;
    private final String mText;
    private final User mSender;
    private String mSenderImage;
    private final Date mDate;

    public Message(UUID chatId, User sender, String text) {
        mChatId = chatId;
        mSender = sender;
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
