package com.caichengxin.chatroom;

import android.support.v4.app.FragmentManager;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.widget.FrameLayout;

/**
 * Created by caiche on 2014/11/2.
 */
public abstract class SingleFragmentActivity extends FragmentActivity
{
    protected static final String FRAGMENT_TAG =
            "SingleFragmentActivity.Fragment";

    protected abstract Fragment createFragment();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        FrameLayout fl = new FrameLayout(this);
        fl.setId(R.id.fragmentContainer);
        setContentView(fl);

        FragmentManager fm = getSupportFragmentManager();
        Fragment fragment = fm.findFragmentById(R.id.fragmentContainer);

        if (fragment == null) {
            fragment = createFragment();
            fm.beginTransaction()
                    .add(R.id.fragmentContainer, fragment)
                    .commit();
        }
    }
}
