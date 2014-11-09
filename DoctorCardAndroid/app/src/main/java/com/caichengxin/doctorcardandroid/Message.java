package com.caichengxin.doctorcardandroid;

import android.graphics.Point;

import java.util.Date;
import java.util.UUID;


public class Message
{
    private long mId;
    private String  mMediaText;
    private String mMediaType;
    private User mAuthor;

    public Message(long id, User author) {
        mId = id;
        mAuthor = author;
    }

    public long getId() {
        return mId;
    }

    public void setId(long id) {
        mId = id;
    }

    public String getMediaText() {
        return mMediaText;
    }

    public void setMediaText(String mediaText) {
        mMediaText = mediaText;
    }

    public String getMediaType() {
        return mMediaType;
    }

    public void setMediaType(String mediaType) {
        mMediaType = mediaType;
    }


    public String toString() {
        if (mMediaType.equals("text"))
            return mMediaText;
        else
            return "<" + mMediaType + ">";
    }

}
