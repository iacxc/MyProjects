package com.caichengxin.simplechat;

import android.app.ActionBar;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;

import java.util.UUID;


public class ChatActivity extends ActionBarActivity {

    public static final String EXTRA_ID = "simplechat.ChatActivity.ID";

    private ListView mLvMessages;
    private EditText mEditSend;
    private Button mButtonSend;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.chat_detail);

        UUID chatId = (UUID)getIntent().getSerializableExtra(EXTRA_ID);
        Chat chat = ChatLab.get(this).getChat(chatId);

        if (chat != null)
            setTitle(chat.getName());

        ActionBar actionBar = getActionBar();
        if (actionBar != null)
            actionBar.setDisplayHomeAsUpEnabled(true);

        mLvMessages = (ListView)findViewById(R.id.list_message);

        mEditSend = (EditText)findViewById(R.id.edit_send);
        mButtonSend = (Button)findViewById(R.id.button_send);

    }


    /*
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
    */
}
