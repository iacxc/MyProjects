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
import android.widget.ListView;

import java.util.ArrayList;
import java.util.UUID;

/**
 * Created by caiche on 2014/11/2.
 */
public class ChatFragment extends Fragment {
    public static final String TAG = "chatroom.ChaFragment";

    public static final String EXTRA_ID = "chatroom.ID";

    private ListView mLvChatMessages;
    private Button mButtonSend;
    private EditText mEditMessage;
    private ArrayList<Message> mChatMessages;

    public static ChatFragment newInstance(UUID roomId)
    {
        Bundle args = new Bundle();
        args.putSerializable(EXTRA_ID, roomId);

        ChatFragment fragment =  new ChatFragment();
        fragment.setArguments(args);

        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);

        UUID chatId = (UUID)getArguments().getSerializable(EXTRA_ID);
        Chat chat = ChatLab.get(getActivity()).getChat(chatId);

        if (chat != null)
            getActivity().setTitle(chat.getName());

        mChatMessages = MessageLab.get(getActivity()).getChatMessageList();
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

        final ChatMessageAdapter adapter = new ChatMessageAdapter(mChatMessages);
        mLvChatMessages = (ListView)view.findViewById(R.id.list_message);
        mLvChatMessages.setAdapter(adapter);

        mEditMessage = (EditText)view.findViewById(R.id.edit_message);
        mButtonSend = (Button)view.findViewById(R.id.button_send);
        mButtonSend.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                Message message = new Message();
//                message.setSender(new User("person1"));
                message.setText(mEditMessage.getText().toString());

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
