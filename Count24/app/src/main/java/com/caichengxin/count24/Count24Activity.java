package com.caichengxin.count24;

import android.app.Activity;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import java.util.Date;
import java.util.List;
import java.util.Stack;


public class Count24Activity extends Activity {

    private Date mStartDate;
    private int[][] mDrawables = new int[][]{
        {R.drawable.c1_c, R.drawable.c1_d, R.drawable.c1_h, R.drawable.c1_s},
        {R.drawable.c2_c, R.drawable.c2_d, R.drawable.c2_h, R.drawable.c2_s},
        {R.drawable.c3_c, R.drawable.c3_d, R.drawable.c3_h, R.drawable.c3_s},
        {R.drawable.c4_c, R.drawable.c4_d, R.drawable.c4_h, R.drawable.c4_s},
        {R.drawable.c5_c, R.drawable.c5_d, R.drawable.c5_h, R.drawable.c5_s},
        {R.drawable.c6_c, R.drawable.c6_d, R.drawable.c6_h, R.drawable.c6_s},
        {R.drawable.c7_c, R.drawable.c7_d, R.drawable.c7_h, R.drawable.c7_s},
        {R.drawable.c8_c, R.drawable.c8_d, R.drawable.c8_h, R.drawable.c8_s},
        {R.drawable.c9_c, R.drawable.c9_d, R.drawable.c9_h, R.drawable.c9_s},
        {R.drawable.c10_c, R.drawable.c10_d,
                R.drawable.c10_h, R.drawable.c10_s},
    };

    private ImageView[] mImages;
    private EditText[] mEditNumbers;
    private TextView mTextAnswer, mTextInput;
    private Button mButtonDeal, mButtonInput, mButtonGetAnswer;
    private LinearLayout mLayoutNumbers;

    private Stack<String> mStackOperators = new Stack<String>();
    private boolean mStarted = false;

    private boolean mAutomatic = true;

    private int pickCardId(int n) {
        int index = (int)(Math.random() * 3);
        return mDrawables[n-1][index];
    }


    public void clearInput() {
        mStartDate = new Date();
        mTextInput.setText("");

        mTextAnswer.setText("");

        mButtonGetAnswer.setEnabled(true);

        if (mAutomatic) {
            mLayoutNumbers.setVisibility(View.INVISIBLE);
            mTextInput.setVisibility(View.VISIBLE);
            mButtonGetAnswer.setText(R.string.label_check_answer);
        }
        else {
            mLayoutNumbers.setVisibility(View.VISIBLE);
            mTextInput.setVisibility(View.INVISIBLE);
            mButtonGetAnswer.setText(R.string.label_show_answer);
        }

        mButtonGetAnswer.setVisibility(mStarted ? View.VISIBLE : View.INVISIBLE);
    }


    private void updateInput() {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (String item : mStackOperators)
        {
            if (first)
                first = false;
            else
                sb.append(" ");
            sb.append(item);
        }

        mTextInput.setText(sb.toString());
    }


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_count24);

        mLayoutNumbers = (LinearLayout)findViewById(R.id.layout_numbers);

        mTextInput = (TextView)findViewById(R.id.text_input);
        mTextAnswer = (TextView)findViewById(R.id.text_answer);

        ImageView image1 = (ImageView)findViewById(R.id.image1);
        ImageView image2 = (ImageView)findViewById(R.id.image2);
        ImageView image3 = (ImageView)findViewById(R.id.image3);
        ImageView image4 = (ImageView)findViewById(R.id.image4);
        mImages = new ImageView [] {image1, image2, image3, image4};

        EditText text1 = (EditText)findViewById(R.id.text1);
        EditText text2 = (EditText)findViewById(R.id.text2);
        EditText text3 = (EditText)findViewById(R.id.text3);
        EditText text4 = (EditText)findViewById(R.id.text4);
        mEditNumbers = new EditText[]{text1, text2, text3, text4};

        text1.addTextChangedListener(new CardTextWatcher(image1));
        text2.addTextChangedListener(new CardTextWatcher(image2));
        text3.addTextChangedListener(new CardTextWatcher(image3));
        text4.addTextChangedListener(new CardTextWatcher(image4));

        image1.setOnClickListener(new NumberOnClickListener(text1));
        image2.setOnClickListener(new NumberOnClickListener(text2));
        image3.setOnClickListener(new NumberOnClickListener(text3));
        image4.setOnClickListener(new NumberOnClickListener(text4));

        mButtonDeal = (Button)findViewById(R.id.button_deal);
        mButtonDeal.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                mAutomatic = true;
                mStarted = true;
                clearInput();

                for(int i=0; i<4; i++) {
                    int index = (int)(Math.random()*10);
                    mEditNumbers[i].setText(String.valueOf(index + 1));
                }



            }
        });

        mButtonInput = (Button)findViewById(R.id.button_input);
        mButtonInput.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mAutomatic = false;
                mStarted = true;
                clearInput();

                for(int i=0; i<4; i++) {
                    mEditNumbers[i].setText("");
                }
            }
        });

        mButtonGetAnswer = (Button)findViewById(R.id.button_answer);
        mButtonGetAnswer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (mAutomatic) {  // check result
                    Toast.makeText(Count24Activity.this, R.string.wrong,
                            Toast.LENGTH_SHORT).show();

                }
                else {
                    for (EditText e : mEditNumbers) {
                        if (e.length() == 0)
                            return;
                    }
                    List<String> resultList = Calculate.easyCount(
                        Integer.valueOf(mEditNumbers[0].getText().toString()),
                        Integer.valueOf(mEditNumbers[1].getText().toString()),
                        Integer.valueOf(mEditNumbers[2].getText().toString()),
                        Integer.valueOf(mEditNumbers[3].getText().toString()));

                    if (resultList.size() > 0) {
                        Date now = new Date();
                        String format_answer = getResources()
                                .getString(R.string.format_answer);
                        mTextAnswer.setText(String.format(format_answer,
                                resultList.get(0),
                                now.getTime() - mStartDate.getTime()));
                    } else {
                        Toast.makeText(Count24Activity.this, R.string.no_answer,
                                Toast.LENGTH_SHORT).show();
                        mTextAnswer.setText("");
                    }
                }

                mStarted = false;
                clearInput();
            }
        });

        ImageView imageAdd = (ImageView)findViewById(R.id.image_add);
        imageAdd.setOnClickListener(new OperatorOnClickListener("+"));

        ImageView imageSub = (ImageView)findViewById(R.id.image_sub);
        imageSub.setOnClickListener(new OperatorOnClickListener("-"));

        ImageView imageMul = (ImageView)findViewById(R.id.image_mul);
        imageMul.setOnClickListener(new OperatorOnClickListener("*"));

        ImageView imageDiv = (ImageView)findViewById(R.id.image_div);
        imageDiv.setOnClickListener(new OperatorOnClickListener("/"));

        ImageView imageLeftP = (ImageView)findViewById(R.id.image_leftp);
        imageLeftP.setOnClickListener(new OperatorOnClickListener("("));

        ImageView imageRightP = (ImageView)findViewById(R.id.image_rightp);
        imageRightP.setOnClickListener(new OperatorOnClickListener(")"));

        ImageView imageDel = (ImageView)findViewById(R.id.image_del);
        imageDel.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (! mStarted) return;

                if (mStackOperators.size() > 0) {
                    mStackOperators.pop();
                    updateInput();
                }
            }
        });

        clearInput();
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
            case  R.id.action_settings:
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    private class CardTextWatcher implements TextWatcher {
        private ImageView mImage;
        public CardTextWatcher(ImageView image) {
            mImage = image;
        }

        @Override
        public void beforeTextChanged(CharSequence s,
                                      int start, int count, int after) { }

        @Override
        public void onTextChanged(CharSequence s,
                                  int start, int before, int count) {
            if (s.length() == 0) return;

            int number = Integer.valueOf(s.toString());
            if (number > 0 && number <= 10)
                mImage.setImageResource(pickCardId(number));
        }

        @Override
        public void afterTextChanged(Editable s) {   }
    }

    private class OperatorOnClickListener implements View.OnClickListener {
        private String mOp;
        public OperatorOnClickListener(String op) {
            mOp = op;
        }

        @Override
        public void onClick(View v) {
            if (! mStarted) return;

            mStackOperators.push(mOp);
            updateInput();
        }
    }

    private class NumberOnClickListener implements View.OnClickListener {
        private TextView mTextView;
        public NumberOnClickListener(TextView textView) {
            mTextView = textView;
        }

        @Override
        public void onClick(View v) {
            if (! mStarted) return;

            mStackOperators.push(mTextView.getText().toString());
            updateInput();
        }
    }

}
