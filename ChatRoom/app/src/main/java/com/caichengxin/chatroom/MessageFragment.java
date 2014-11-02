package com.caichengxin.chatroom;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;

import java.util.ArrayList;

/**
 * Created by caiche on 2014/11/2.
 */
public class MessageFragment extends Fragment {
    public static final String EXTRA_TITLE = "chatroom.CHAT_TITLE";

    private ListView mLvChatMessages;
    private Button mButtonSend;
    private EditText mEditMessage;
    private ArrayList<Message> mChatMessages;

    public static MessageFragment newInstance(String chatTitle)
    {
        Bundle args = new Bundle();
        args.putSerializable(EXTRA_TITLE, chatTitle);

        MessageFragment fragment =  new MessageFragment();
        fragment.setArguments(args);

        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        String chatTitle = (String)getArguments().getSerializable(EXTRA_TITLE);
        getActivity().setTitle(chatTitle);

        mChatMessages = MessageLab.get(getActivity()).getChatMessageList();
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState)
    {
        View view = inflater.inflate(R.layout.fragment_chat_detail,
                container, false);

        final ChatMessageAdapter adapter = new ChatMessageAdapter(mChatMessages);
        mLvChatMessages = (ListView)view.findViewById(R.id.list_message);
        mLvChatMessages.setAdapter(adapter);

        mEditMessage = (EditText)view.findViewById(R.id.edit_message);
        mButtonSend = (Button)view.findViewById(R.id.button_send);
        mButtonSend.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                Message message = new Message("me",
                        mEditMessage.getText().toString());

                mChatMessages.add(message);
                adapter.notifyDataSetChanged();
                mEditMessage.setText(null);
            }
        });

        return view;
    }


    private class ChatMessageAdapter extends ArrayAdapter<Message>
    {

        public ChatMessageAdapter(ArrayList<Message> chatMessageList) {
            super(getActivity(), android.R.layout.simple_list_item_1,
                    chatMessageList);
        }
    }
}
