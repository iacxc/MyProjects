package com.caichengxin.chatroom;

import java.util.UUID;

/**
 * Created by caiche on 2014/11/3.
 */
public class Person
{
    private UUID mId;
    private String mName;

    public Person(String name)
    {
        mId = UUID.randomUUID();
        mName = name;
    }

    public UUID getId() {
        return mId;
    }

    public String getName() {
        return mName;
    }

    public void setName(String name) {
        mName = name;
    }
}
