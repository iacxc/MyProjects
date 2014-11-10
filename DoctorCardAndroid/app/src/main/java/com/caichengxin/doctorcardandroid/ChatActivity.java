package com.caichengxin.doctorcardandroid;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.ConsoleMessage;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;

import java.util.ArrayList;


public class ChatActivity extends Activity {

    private static final String TAG = "doctorcardandroid.ChatActivity";
    public static final String EXTRA_ID = "doctorcardandroid.ID";

    private ArrayList<Message> mMessageList;

    private ListView mLvMessages;
    private EditText mEditMessage;
    private Button mButtonSend;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_chat);

        //Intent intent = getIntent();
        long id = getIntent().getLongExtra(EXTRA_ID, -1);

        Log.d(TAG, String.valueOf(id));

        Chat chat = ChatLab.get().findChatById(id);

        setTitle(chat.getName());

        mMessageList = new ArrayList<Message>();
        loadMessages(chat);

        MessageAdapter adapter = new MessageAdapter(mMessageList);
        mLvMessages = (ListView)findViewById(R.id.list_message);
        mLvMessages.setAdapter(adapter);

        mEditMessage = (EditText)findViewById(R.id.edit_message);

        mButtonSend = (Button)findViewById(R.id.button_send);
        mButtonSend.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                String txtMessage = mEditMessage.getText().toString();

                Log.d(TAG, "Sending " + txtMessage);
                mEditMessage.setText("");
            }
        });
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_chat, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    private void loadMessages(Chat chat) {
        long id = chat.getId();

        //get all message for the chat id
        //for debug purpose, generate some random message
        for(int i=0; i < 30; i++) {
            Message msg = new Message(i, UserLab.get().getRandomUser());
            msg.setMediaText("Message #" + i);
            if ((int)(Math.random() * 2) == 1)
                msg.setMediaType("text");
            else
                msg.setMediaType("movie");

            mMessageList.add(msg);
        }
    }

    private class MessageAdapter extends ArrayAdapter<Message>
    {
        public MessageAdapter(ArrayList<Message> messages) {
            super(ChatActivity.this, 0, messages);
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent)
        {
            if (convertView == null)
                convertView = getLayoutInflater().inflate(
                        R.layout.list_item_message, null);

            Message msg = getItem(position);

            TextView textMessage =
                    (TextView)convertView.findViewById(R.id.text_message);
            textMessage.setText(msg.toString());

            return convertView;
        }

    }
}
