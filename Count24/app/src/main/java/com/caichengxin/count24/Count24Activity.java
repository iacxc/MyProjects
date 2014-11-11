package com.caichengxin.count24;

import android.app.ActionBar;
import android.content.res.Resources;
import android.graphics.drawable.BitmapDrawable;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.text.Editable;
import android.text.Layout;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TableLayout;
import android.widget.TableRow;
import android.widget.TextView;
import android.widget.Toast;

import java.util.Date;
import java.util.List;


public class Count24Activity extends ActionBarActivity {

    private Date mStartDate;
    private int[] mDrawables = new int[]{R.drawable.c1,
            R.drawable.c2,
            R.drawable.c3,
            R.drawable.c4,
            R.drawable.c5,
            R.drawable.c6,
            R.drawable.c7,
            R.drawable.c8,
            R.drawable.c9,
            R.drawable.c10 };

    private ImageView[]  mImages;
    private EditText[] mEditNumbers;
    private TextView mTextAnswer;
    private LinearLayout mLayoutNumbers;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_count24);

        mLayoutNumbers = (LinearLayout)findViewById(R.id.layout_numbers);
//        ViewGroup.LayoutParams linearParams = mLayoutNumbers.getLayoutParams();
//        linearParams.height = 0;
//        mLayoutNumbers.setLayoutParams(linearParams);
        mLayoutNumbers.setVisibility(View.INVISIBLE);

        final ImageView image1 = (ImageView)findViewById(R.id.image1);
        final ImageView image2 = (ImageView)findViewById(R.id.image2);
        final ImageView image3 = (ImageView)findViewById(R.id.image3);
        final ImageView image4 = (ImageView)findViewById(R.id.image4);
        mImages = new ImageView [] {image1, image2, image3, image4};

        EditText text1 = (EditText)findViewById(R.id.text1);
        EditText text2 = (EditText)findViewById(R.id.text2);
        EditText text3 = (EditText)findViewById(R.id.text3);
        EditText text4 = (EditText)findViewById(R.id.text4);
        mEditNumbers = new EditText[]{text1, text2, text3, text4};

        text1.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                int index = Integer.valueOf(s.toString());
                if (index > 0 && index <= 10)
                    image1.setImageResource(mDrawables[index-1]);
            }

            @Override
            public void afterTextChanged(Editable s) {

            }
        });


        mTextAnswer = (TextView)findViewById(R.id.text_answer);

        Button buttonDeal = (Button)findViewById(R.id.button_deal);
        buttonDeal.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                for(int i=0; i<4; i++) {
                    int index = (int)(Math.random()*10);
                    //mImages[i].setImageResource(mDrawables[index]);
                    mEditNumbers[i].setText(String.valueOf(index + 1));
                }
                mStartDate = new Date();
                mTextAnswer.setText("");

            }
        });

        Button buttonInput = (Button)findViewById(R.id.button_input);
        buttonInput.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
//                ViewGroup.LayoutParams linearParams =
//                        mLayoutNumbers.getLayoutParams();
//                linearParams.height = ViewGroup.LayoutParams.WRAP_CONTENT;
//                mLayoutNumbers.setLayoutParams(linearParams);

                mLayoutNumbers.setVisibility(View.VISIBLE);
            }
        });

        Button buttonGetAnswer = (Button)findViewById(R.id.button_answer);
        buttonGetAnswer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                List<String> resultList = Calculate.easyCount(
                        new int[] {Integer.valueOf(mEditNumbers[0].getText().toString()),
                                   Integer.valueOf(mEditNumbers[1].getText().toString()),
                                   Integer.valueOf(mEditNumbers[2].getText().toString()),
                                   Integer.valueOf(mEditNumbers[3].getText().toString())});

                if (resultList.size() > 0) {
                    Date now = new Date();
                    String format_answer = getResources().getString(R.string.format_answer);
                    mTextAnswer.setText(String.format(format_answer,
                            resultList.get(0),
                            now.getTime() - mStartDate.getTime()));
                }
                else {
                    Toast.makeText(Count24Activity.this, R.string.no_answer,
                            Toast.LENGTH_SHORT).show();
                    mTextAnswer.setText("");
                }
            }
        });
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.count24, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        switch (item.getItemId()) {
            case R.id.new_game:
                for(int i=0; i< 4; i++) {
                    mEditNumbers[i].setText("");
                    mImages[i].setImageResource(R.drawable.back);
                }
//                ViewGroup.LayoutParams linearParams = mLayoutNumbers.getLayoutParams();
//                linearParams.height = 0;
//                mLayoutNumbers.setLayoutParams(linearParams);
                mLayoutNumbers.setVisibility(View.INVISIBLE);

                mTextAnswer.setText("");
                return true;
            case  R.id.action_settings:
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }
}
