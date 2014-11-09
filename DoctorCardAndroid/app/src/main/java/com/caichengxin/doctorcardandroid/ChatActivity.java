package com.caichengxin.doctorcardandroid;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;

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

        mLvMessages = (ListView)findViewById(R.id.list_message);

        mEditMessage = (EditText)findViewById(R.id.edit_message);

        mButtonSend = (Button)findViewById(R.id.button_send);
        mButtonSend.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
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

    }
}
