package com.caichengxin.chatroom;

import android.os.Bundle;
import android.app.Fragment;
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


public class ChatFragment extends Fragment {
    public static final String TAG = "chatroom.ChatFragment";

    public static final String EXTRA_ID = "chatroom.Chat_ID";

    private Chat mChat;
    private ArrayList<Message> mChatMessages;

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
//        setHasOptionsMenu(true);

        UUID chatId = (UUID)getArguments().getSerializable(EXTRA_ID);
        mChat = ChatLab.get().getChat(chatId);

        MessageLab msgLab = MessageLab.get();

        if (mChat != null) {
            getActivity().setTitle(mChat.getName());

            msgLab.init(mChat);
        }

        mChatMessages =  msgLab.getChatMessageList();

    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState)
    {
        View view = inflater.inflate(R.layout.fragment_chat,
                container, false);

        //ActionBarActivity activity = (ActionBarActivity)getActivity();
        //activity.getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        final MessageAdapter adapter = new MessageAdapter(mChatMessages);

        ListView lvChatMessages = (ListView)view.findViewById(R.id.list_message);
        lvChatMessages.setAdapter(adapter);

        mEditMessage = (EditText)view.findViewById(R.id.edit_message);
        Button buttonSend = (Button)view.findViewById(R.id.button_send);
        buttonSend.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                Message message = new Message(mChat.getId(),
                        ChatListActivity.ME, mEditMessage.getText().toString());

                mChatMessages.add(message);

                adapter.notifyDataSetChanged();

                mEditMessage.setText(null);
            }
        });

        mEditMessage.requestFocus();
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
