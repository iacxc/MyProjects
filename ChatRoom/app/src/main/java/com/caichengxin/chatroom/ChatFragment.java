package com.caichengxin.chatroom;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.app.ActionBar;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.UUID;

/**
 * Created by caiche on 2014/11/2.
 */
public class ChatFragment extends Fragment {
    public static final String TAG = "chatroom.ChaFragment";

    public static final String EXTRA_ID = "chatroom.ID";

    private Chat mChat;
    private ArrayList<Message> mChatMessages;

    private ListView mLvChatMessages;
    private Button mButtonSend;
    private EditText mEditMessage;

    public static ChatFragment newInstance(UUID chatId)
    {
        Bundle args = new Bundle();
        args.putSerializable(EXTRA_ID, chatId);

        ChatFragment fragment =  new ChatFragment();
        fragment.setArguments(args);

        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);

        UUID chatId = (UUID)getArguments().getSerializable(EXTRA_ID);
        mChat = ChatLab.get(getActivity()).getChat(chatId);

        if (mChat != null) {
            getActivity().setTitle(mChat.getName());

            MessageLab.get(getActivity()).init(mChat);
        }

        mChatMessages =  MessageLab.get(getActivity()).getChatMessageList();

    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState)
    {
        View view = inflater.inflate(R.layout.fragment_chat_detail,
                container, false);

        ActionBar actionBar = getActivity().getActionBar();
        if (actionBar != null)
            actionBar.setDisplayHomeAsUpEnabled(true);


        final  MessageAdapter adapter = new MessageAdapter(mChatMessages);

        mLvChatMessages = (ListView)view.findViewById(R.id.list_message);
        mLvChatMessages.setAdapter(adapter);

        mEditMessage = (EditText)view.findViewById(R.id.edit_message);
        mButtonSend = (Button)view.findViewById(R.id.button_send);
        mButtonSend.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                Message message = new Message(mChat.getId(),
                        ChatListActivity.ME, mEditMessage.getText().toString());

                mChatMessages.add(message);

                adapter.notifyDataSetChanged();

                mEditMessage.setText(null);
            }
        });

        return view;
    }


    private class MessageAdapter extends ArrayAdapter<Message>
    {

        public MessageAdapter(ArrayList<Message> messageList) {
            super(getActivity(), 0,   messageList);
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            if (convertView == null)
                convertView = getActivity().getLayoutInflater()
                        .inflate(R.layout.list_item_message, null);

            Message msg = getItem(position);

            TextView textMessage =
                    (TextView)convertView.findViewById(R.id.text_message);
            textMessage.setText(msg.getText());

            ImageView imgOther =
                    (ImageView)convertView.findViewById(R.id.image_other);
            ImageView imgMe =
                    (ImageView)convertView.findViewById(R.id.image_me);

            if (msg.getSender().equals(ChatListActivity.ME)) {
                imgMe.setVisibility(View.VISIBLE);
                imgOther.setVisibility(View.INVISIBLE);
            }
            else {
                imgMe.setVisibility(View.INVISIBLE);
                imgOther.setVisibility(View.VISIBLE);
            }

            return convertView;
        }
    }
}
