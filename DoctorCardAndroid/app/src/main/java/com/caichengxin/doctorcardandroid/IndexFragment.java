package com.caichengxin.doctorcardandroid;


import android.app.Activity;
import android.os.Bundle;
import android.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;


public class IndexFragment extends Fragment {

    OnIndexPressedListener mCallback;

    public interface OnIndexPressedListener {
        public void onChatButtonPressed();
        public void onFriendButtonPressed();
        public void onDiscoverButtonPressed();
        public void onMyselfButtonPressed();
    }

    @Override
    public void onAttach(Activity activity)
    {
        super.onAttach(activity);

        try {
            mCallback = (OnIndexPressedListener)activity;
        }
        catch (ClassCastException e) {
            throw new ClassCastException(activity.toString()
            + " must implement OnIndexPressedListener");
        }
    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View view = inflater.inflate(R.layout.fragment_index, container, false);

        ImageButton buttonChat = (ImageButton)view.findViewById(R.id.button_chat);
        buttonChat.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mCallback.onChatButtonPressed();
            }
        });

        ImageButton buttonFriend = (ImageButton)view.findViewById(R.id.button_friend);
        buttonFriend.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mCallback.onFriendButtonPressed();
            }
        });

        ImageButton buttonDiscover = (ImageButton)view.findViewById(R.id.button_discovery);
        buttonDiscover.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mCallback.onDiscoverButtonPressed();
            }
        });

        ImageButton buttonMyself = (ImageButton)view.findViewById(R.id.button_myself);
        buttonMyself.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mCallback.onMyselfButtonPressed();
            }
        });
        return view;
    }


}
