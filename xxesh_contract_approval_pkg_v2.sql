set define OFF;
CREATE OR replace PACKAGE BODY xxesh_contract_approval_pkg
AS
  g_contract_gm_bu_role  VARCHAR2(100);
  g_contract_sd_scm_role VARCHAR2(100);
  g_contract_vp_scm_role VARCHAR2(100);
  g_contract_ceo_role    VARCHAR2(100);
  g_conc_id              NUMBER := fnd_global.conc_request_id;
  FUNCTION Working_days_derivation (p_reminder     VARCHAR2,
                                    p_date_started DATE)
  RETURN DATE
  AS
    l_lap_days     NUMBER;
    l_cnt          NUMBER := 0;
    l_date_started VARCHAR2(30);
  BEGIN
      SELECT lookup_code
      INTO   l_lap_days
      FROM   fnd_lookup_values
      WHERE  lookup_type = 'XXESH_CONTRACT_WF_TIMEOUT_DAYS'
             AND LANGUAGE = 'US'
             AND meaning = p_reminder;

      l_date_started := To_char(p_date_started, 'DD-MON-RRRR HH24:MI:SS');

      FOR i IN (SELECT To_date(a.dates, 'DD-MON-RRRR HH24:MI:SS') dates
                FROM   (SELECT CASE
                                 WHEN Trim(To_char(To_date(l_date_started,
                                                   'DD-MON-RRRR HH24:MI:SS')
                                                   + LEVEL, 'day')) IN
                                      (SELECT Lower(lookup_code)
                                       FROM   fnd_lookup_values
                                       WHERE
                                             lookup_type =
                                             'XXESH_CONTRACT_WF_TIMEOUT_DAYS'
                                             AND description = 'WEEKEND'
                                             AND LANGUAGE = 'US') THEN 1
                                 ELSE 0
                               END     week_days,
                               To_date(l_date_started, 'DD-MON-RRRR HH24:MI:SS')
                               + LEVEL dates
                        FROM   dual
                        CONNECT BY LEVEL <= 30) a
                WHERE  week_days != 1
                       AND NOT EXISTS (SELECT 1
                                       FROM   fnd_lookup_values
                                       WHERE
                               lookup_type = 'XXESH_CONTRACT_HOLIDAYS'
                               AND LANGUAGE = 'US'
                               AND description = 'HOLIDAYS'
                               AND Trunc(To_date(a.dates)) = Trunc(
                                   start_date_active))
                ORDER  BY 1) LOOP
          l_cnt := l_cnt + 1;

          IF l_cnt = l_lap_days THEN
            RETURN To_date(i.dates, 'DD-MON-RRRR HH24:MI:SS');
          END IF;
      END LOOP;
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(123, 'working_days_derivation',
               'Outtermost exception ->'
               ||
               dbms_utility.format_error_backtrace
                                                              ||
               '   and stack  -> '
                                                              ||
               dbms_utility.format_error_stack);

               RETURN l_date_started;
  END working_days_derivation;
  PROCEDURE Timeout_reminders (errbuf  OUT VARCHAR2,
                               retcode OUT NUMBER)
  AS
    l_subject       VARCHAR2(3000) := NULL;
    l_to_address    VARCHAR2(3000) := NULL;
    l_mail_body     VARCHAR2(3000) := NULL;
    l_to_address2   VARCHAR2(3000) := NULL;
    l_mail_body_txt VARCHAR2(3000) := NULL;
  BEGIN
      fnd_file.Put_line(fnd_file.log, 'Inside the logic timeout_reminders');

      FOR i IN (SELECT wn.notification_id
                FROM   wf_item_attribute_values wiav,
                       wf_notifications wn
                WHERE  wiav.item_type = 'XXESHWF'
                       AND message_type = wiav.item_type
                       AND wiav.item_key = wn.item_key
                       AND wiav.text_value = wn.recipient_role
                       AND end_date IS NOT NULL
                       AND EXISTS (SELECT 1
                                   FROM   xxesh_timeout_info info
                                   WHERE  1 = 1
                                          AND info.notification_id =
                                              wn.notification_id)) LOOP
          UPDATE xxesh_timeout_info
          SET    action = 'Y'
          WHERE  notification_id = i.notification_id;
      END LOOP;

      INSERT INTO xxesh_timeout_info
                  (request_id,
                   notification_id,
                   item_key,
                   recipient_role_name,
                   recipient_role,
                   superior_role_name,
                   superior_role,
                   action,
                   notification_begin_date,
                   reminder_date1,
                   reminder_date2,
                   reminder_date3,
                   reminder_date4)
      SELECT g_conc_id,
             wn.notification_id,
             wiav.item_key,
             wiav.name,
             wn.recipient_role,
             (SELECT description
              FROM   fnd_lookup_values
              WHERE  lookup_type = 'XXESH_CONTRACT_WF_TIMEOUT_ROLE'
                     AND LANGUAGE = 'US'
                     AND meaning = wiav.name) actual_role_name,
             Nvl((SELECT text_value
                  FROM   wf_item_attribute_values wiav1
                  WHERE  1 = 1
                         AND wiav1.item_type = wiav.item_type
                         AND wiav.item_key = wiav1.item_key
                         AND wiav1.name = (SELECT description
                                           FROM   fnd_lookup_values
                                           WHERE
                             lookup_type = 'XXESH_CONTRACT_WF_TIMEOUT_ROLE'
                             AND LANGUAGE = 'US'
                             AND meaning = wiav.name)), (SELECT tag
                                                         FROM
             fnd_lookup_values
                                                         WHERE
             lookup_type = 'XXESH_CONTRACT_WF_TIMEOUT_ROLE'
             AND LANGUAGE = 'US'
             AND meaning = wiav.name))        role_name,
             'N',
             wn.begin_date,
             Working_days_derivation('1st Reminder', wn.begin_date),
             Working_days_derivation('2nd Reminder', wn.begin_date),
             Working_days_derivation('3rd Reminder', wn.begin_date),
             Working_days_derivation('4th Reminder', wn.begin_date)
      FROM   wf_item_attribute_values wiav,
             wf_notifications wn
      WHERE  wiav.item_type = 'XXESHWF'
             --AND wiav.ITEM_KEY=  '12308_3'
             AND message_type = wiav.item_type
             AND wiav.item_key = wn.item_key
             AND wiav.text_value = wn.recipient_role
             AND wn.status = 'OPEN'
             AND wn.mail_status = 'MAIL'
             AND NOT EXISTS (SELECT 1
                             FROM   xxesh_timeout_info info
                             WHERE  1 = 1
                                    AND info.notification_id =
                                        wn.notification_id
                            --  AND action='Y'
                            );

      UPDATE xxesh_timeout_info
      SET    reminder_date1 = notification_begin_date + (
                              Trunc(reminder_date1) - Trunc(
                              notification_begin_date) ),
             reminder_date2 = notification_begin_date + (
                              Trunc(reminder_date2) - Trunc(
                              notification_begin_date) ),
             reminder_date3 = notification_begin_date + (
                              Trunc(reminder_date3) - Trunc(
                              notification_begin_date) ),
             reminder_date4 = notification_begin_date + (
                              Trunc(reminder_date4) - Trunc(
                              notification_begin_date) )
      WHERE  request_id = g_conc_id;

      l_subject := NULL;

      l_to_address := NULL;

      l_mail_body := NULL;

      --------1st reminder -----------
      FOR reminder_c IN (SELECT notification_id,
                                recipient_role,
                                item_key
                         FROM   xxesh_timeout_info
                         WHERE  reminder_date1 <= SYSDATE
                                AND actual_reminder_date1 IS NULL
                                AND action = 'N') LOOP
          l_subject := NULL;

          l_to_address := NULL;

          l_mail_body := NULL;

          SELECT email_address
          INTO   l_to_address
          FROM   fnd_user
          WHERE  user_id = (SELECT Max(user_id)
                            FROM   fnd_user
                            WHERE  user_name = reminder_c.recipient_role);

          SELECT text_value
          INTO   l_subject
          FROM   wf_notification_attributes
          WHERE  1 = 1
                 AND notification_id = reminder_c.notification_id
                 AND name = 'NOTIFICATION_SUBJECT';

          l_subject := '1st Reminder :'
                       || l_subject;

          /* l_mail_body := 'This is the contract form reminder notification, A request is pending with '
                         || reminder_c.recipient_role
                         || '. please take an action at the earliest, to avoid escalation.
                       Notification id is '
                         || reminder_c.notification_id; */
          l_mail_body := NULL;

          SELECT text_value
          INTO   l_mail_body
          FROM   wf_item_attribute_values
          WHERE  item_type = 'XXESHWF'
                 AND item_key = reminder_c.item_key
                 AND name = 'NOTIFICATION_BODY';

          /*       l_mail_body :='Hi,
                               Please note that your Contract request No. '||l_field1||' is Approved, please refer to the following information:
                               •  Contract Reference : '||l_field4||'
                               •  Contract Amount : '||l_field5||'
                               •  Contract Category : '||l_field6||'
                               •  Contract period : '||l_field9||'
                               •  Contract Type : '||l_field10||'
                               Thanks,
                               IT Admin'; */
          Call_smtp(reminder_c.notification_id, NULL, l_to_address, l_subject,
          l_mail_body
          , 1);
      END LOOP;

      --------2nd reminder -----------
      FOR reminder_c IN (SELECT notification_id,
                                recipient_role,
                                superior_role,
                                item_key
                         FROM   xxesh_timeout_info
                         WHERE  reminder_date2 <= SYSDATE
                                AND actual_reminder_date2 IS NULL
                                AND action = 'N') LOOP
          l_subject := NULL;

          l_to_address := NULL;

          l_mail_body := NULL;

          l_to_address2 := NULL;

          SELECT email_address
          INTO   l_to_address
          FROM   fnd_user
          WHERE  user_id = (SELECT Max(user_id)
                            FROM   fnd_user
                            WHERE  user_name = reminder_c.recipient_role);

          IF reminder_c.superior_role IS NOT NULL THEN
            SELECT email_address
            INTO   l_to_address2
            FROM   fnd_user
            WHERE  user_id = (SELECT Max(user_id)
                              FROM   fnd_user
                              WHERE  user_name = reminder_c.superior_role);

            l_to_address := l_to_address
                            || ','
                            || l_to_address2;
          END IF;

          SELECT text_value
          INTO   l_subject
          FROM   wf_notification_attributes
          WHERE  1 = 1
                 AND notification_id = reminder_c.notification_id
                 AND name = 'NOTIFICATION_SUBJECT';

          l_subject := '2nd Reminder :'
                       || l_subject;

          SELECT text_value
          INTO   l_mail_body
          FROM   wf_item_attribute_values
          WHERE  item_type = 'XXESHWF'
                 AND item_key = reminder_c.item_key
                 AND name = 'NOTIFICATION_BODY';

          /*       l_mail_body := 'This is the contract form reminder notification, A request is pending with '
                               || reminder_c.recipient_role
                               || '. please take an action at the earliest.
                                   Notification id is '
                               || reminder_c.notification_id;
           */
          Call_smtp(reminder_c.notification_id, NULL, l_to_address, l_subject,
          l_mail_body
          , 2);
      END LOOP;

      --------3rd reminder -----------
      FOR reminder_c IN (SELECT notification_id,
                                recipient_role,
                                item_key
                         FROM   xxesh_timeout_info
                         WHERE  reminder_date3 <= SYSDATE
                                AND actual_reminder_date3 IS NULL
                                AND action = 'N') LOOP
          l_subject := NULL;

          l_to_address := NULL;

          l_mail_body := NULL;

          SELECT email_address
          INTO   l_to_address
          FROM   fnd_user
          WHERE  user_id = (SELECT Max(user_id)
                            FROM   fnd_user
                            WHERE  user_name = reminder_c.recipient_role);

          SELECT text_value
          INTO   l_subject
          FROM   wf_notification_attributes
          WHERE  1 = 1
                 AND notification_id = reminder_c.notification_id
                 AND name = 'NOTIFICATION_SUBJECT';

          l_subject := '3rd Reminder :'
                       || l_subject;

          SELECT text_value
          INTO   l_mail_body
          FROM   wf_item_attribute_values
          WHERE  item_type = 'XXESHWF'
                 AND item_key = reminder_c.item_key
                 AND name = 'NOTIFICATION_BODY';

          /*       l_mail_body := 'This is the contract form reminder notification, A request is pending with '
                               || reminder_c.recipient_role
                               || '. please take an action at the earliest, to avoid escalation.
                                   Notification id is '
                               || reminder_c.notification_id; */
          Call_smtp(reminder_c.notification_id, NULL, l_to_address, l_subject,
          l_mail_body
          , 3);
      END LOOP;

      --------4th reminder -----------
      FOR reminder_c IN (SELECT notification_id,
                                recipient_role,
                                superior_role,
                                item_key
                         FROM   xxesh_timeout_info
                         WHERE  reminder_date4 <= SYSDATE
                                AND actual_reminder_date4 IS NULL
                                AND action = 'N') LOOP
          l_subject := NULL;

          l_to_address := NULL;

          l_mail_body := NULL;

          l_to_address2 := NULL;

          SELECT email_address
          INTO   l_to_address
          FROM   fnd_user
          WHERE  user_id = (SELECT Max(user_id)
                            FROM   fnd_user
                            WHERE  user_name = reminder_c.recipient_role);

          SELECT text_value
          INTO   l_subject
          FROM   wf_notification_attributes
          WHERE  1 = 1
                 AND notification_id = reminder_c.notification_id
                 AND name = 'NOTIFICATION_SUBJECT';

          IF reminder_c.superior_role IS NOT NULL THEN
            SELECT email_address
            INTO   l_to_address2
            FROM   fnd_user
            WHERE  user_id = (SELECT Max(user_id)
                              FROM   fnd_user
                              WHERE  user_name = reminder_c.superior_role);

            l_to_address := l_to_address
                            || ','
                            || l_to_address2;
          END IF;

          l_subject := '4th Reminder :'
                       || l_subject;

          SELECT text_value
          INTO   l_mail_body
          FROM   wf_item_attribute_values
          WHERE  item_type = 'XXESHWF'
                 AND item_key = reminder_c.item_key
                 AND name = 'NOTIFICATION_BODY';

          /*       l_mail_body := 'This is the contract form reminder notification, A request is pending with '
                               || reminder_c.recipient_role
                               || '. please take an action at the earliest.
                                   Notification id is '
                               || reminder_c.notification_id; */
          Call_smtp(reminder_c.notification_id, NULL, l_to_address, l_subject,
          l_mail_body
          , 4);
      END LOOP;
  EXCEPTION
    WHEN OTHERS THEN
               errbuf := 'Please check the log';

               retcode := 2;

               fnd_file.Put_line(fnd_file.log,
               'TIMEOUT_REMINDERS Outtermost Exception backtrace ->'
               || dbms_utility.format_error_backtrace
               || '   and stack  -> '
               || dbms_utility.format_error_stack);
  END timeout_reminders;
  PROCEDURE Delete_null_rec (p_item_type IN VARCHAR2,
                             p_item_key  IN VARCHAR2)
  AS
    PRAGMA autonomous_transaction;
    l_status VARCHAR2(100);
  BEGIN
      DELETE FROM xxesh_customer_noti_comments
      WHERE  1 = 1
             AND item_type = p_item_type
             AND item_key = p_item_key
             AND user_comments IS NULL;

      BEGIN
          BEGIN
              SELECT responder
                     || ' action '
                     || text_value
              INTO   l_status
              FROM   (SELECT a.notification_id,
                             a.responder,
                             b.text_value
                      FROM   wf_notifications a,
                             wf_notification_attributes b
                      WHERE  1 = 1
                             AND a.notification_id = b.notification_id
                             AND a.message_type = p_item_type
                             AND a.item_key = p_item_key
                             AND b.name = 'RESULT'
                             AND b.text_value IS NOT NULL
                      ORDER  BY 1 DESC)
              WHERE  ROWNUM = 1;
          EXCEPTION
              WHEN OTHERS THEN
                l_status := NULL;
          END;

          /*        write_cust_log(123, 'APPROVAL_STATUS',' p_item_type ->'
                              || p_item_type
                              || ' $  P_ITEM_KEY ->'
                              || p_item_key
                              || '      and    l_status'
                              || l_status); */
          IF l_status IS NULL THEN
            UPDATE xxesh_contract_headers_t
            SET    approval_status = Upper(l_status)
            WHERE  wf_item_key = p_item_key
                   AND wf_item_type = p_item_type;
          END IF;
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'APPROVAL_STATUS', ' p_item_type ->'
                                                   || p_item_type
                                                   || ' $  P_ITEM_KEY ->'
                                                   || p_item_key
                                                   || '   '
                                                   || SQLERRM);
      END;

      COMMIT;
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(123, 'DELETE_NULL_REC', 'p_item_type ->'
                                                      || p_item_type
                                                      || ' $  P_ITEM_KEY ->'
                                                      || p_item_key
                                                      || '   '
                                                      || SQLERRM);
  END delete_null_rec;
  PROCEDURE Approver_info (p_department_name IN VARCHAR2,
                           p_item_type       IN VARCHAR2,
                           p_item_key        IN VARCHAR2,
                           o_user_name       OUT VARCHAR2,
                           o_mail_id         OUT VARCHAR2,
                           o_full_name       OUT VARCHAR2)
  AS
    l_vac_approver VARCHAR2(40);
  BEGIN
      SELECT DISTINCT fndu.user_name,
                      fndu.email_address,
                      papf.full_name
      INTO   o_user_name, o_mail_id, o_full_name
      FROM   fnd_lookup_values_vl fv,
             fnd_lookup_types_vl fl,
             fnd_lookup_values_vl fv1,
             fnd_lookup_types_vl fl1,
             hr_all_organization_units hr,
             xxesh_contract_headers_t h,
             per_all_people_f papf,
             fnd_user fndu,
             wf_roles wr
      WHERE  fv.lookup_type = 'XXESH_CONTRACT_APPROVAL_LIST'
             AND fv.lookup_type = fl.lookup_type
             AND hr.name = fv1.meaning
             AND fv.description = p_department_name--'BU/HOD'
             AND fv.tag = fv1.lookup_code
             AND fv.enabled_flag = 'Y'
             AND fv1.lookup_type = 'XXESH_OU_LIST'
             AND fv1.lookup_type = fl1.lookup_type
             AND h.org_id = hr.organization_id
             AND fv1.enabled_flag = 'Y'
             AND h.wf_item_key = p_item_key
             AND h.wf_item_type = p_item_type
             AND fndu.employee_id = papf.person_id
             AND fndu.user_id = fv.attribute1
             AND To_char(effective_end_date, 'DD-MON-YY') = To_date('31-DEC-12')
             AND fndu.user_name NOT LIKE '%@%'
             AND fndu.user_name = wr.name
             AND fndu.customer_id IS NULL;

      Delete_null_rec(p_item_type, p_item_key);

      BEGIN
          SELECT action_argument
          INTO   l_vac_approver
          FROM   wf_routing_rules
          WHERE  ROLE = o_user_name
                 AND message_type = 'XXESHWF'
                 AND SYSDATE BETWEEN Trunc(begin_date) AND Trunc(end_date);
      EXCEPTION
          WHEN OTHERS THEN
            l_vac_approver := NULL;
      END;

      INSERT INTO xxesh_contract_kpi_report
                  (item_type,
                   item_key,
                   actual_user,
                   created_by,
                   creation_date,
                   last_updated_by,
                   last_update_date,
                   last_update_login,
                   role_name,
                   delegated_to)
      SELECT 'XXESHWF',
             p_item_key,
             o_user_name,
             fnd_global.user_id,
             SYSDATE,
             fnd_global.user_id,
             SYSDATE,
             fnd_global.login_id,
             p_department_name,
             l_vac_approver
      FROM   dual
      WHERE  1 = 1
             AND NOT EXISTS (SELECT 1
                             FROM   xxesh_contract_kpi_report
                             WHERE  item_type = 'XXESHWF'
                                    AND item_key = p_item_key
                                    AND actual_user = o_user_name
                                    AND role_name = p_department_name
                            -- AND delegated_to = l_vac_approver
                            );
  EXCEPTION
    WHEN OTHERS THEN
               Delete_null_rec(p_item_type, p_item_key);

               BEGIN
                   SELECT DISTINCT fndu.user_name,
                                   fndu.email_address,
                                   papf.full_name
                   INTO   o_user_name, o_mail_id, o_full_name
                   FROM   fnd_lookup_values_vl fv,
                          fnd_lookup_types_vl fl,
                          fnd_lookup_values_vl fv1,
                          fnd_lookup_types_vl fl1,
                          hr_all_organization_units hr,
                          xxesh_contract_headers_t h,
                          per_all_people_f papf,
                          fnd_user fndu,
                          wf_roles wr
                   WHERE  fv.lookup_type = 'XXESH_CONTRACT_APPROVAL_LIST'
                          AND fv.lookup_type = fl.lookup_type
                          AND hr.name = fv1.meaning
                          AND fv.description = p_department_name--'BU/HOD'
                          AND fv.tag = fv1.lookup_code
                          AND fv.enabled_flag = 'Y'
                          AND fv1.lookup_type = 'XXESH_OU_LIST'
                          AND fv1.lookup_type = fl1.lookup_type
                          AND h.org_id = hr.organization_id
                          AND fv1.enabled_flag = 'Y'
                          AND h.wf_item_key = p_item_key
                          AND h.wf_item_type = p_item_type
                          AND fndu.employee_id = papf.person_id
                          AND fndu.user_id = fv.attribute1
                          AND To_char(effective_end_date, 'DD-MON-YY') = To_date
                              (
                              '31-DEC-12')
                          AND fndu.user_name NOT LIKE '%@%'
                          AND fndu.user_name = wr.name
                          AND ROWNUM = 1;

                   BEGIN
                       SELECT action_argument
                       INTO   l_vac_approver
                       FROM   wf_routing_rules
                       WHERE  ROLE = o_user_name
                              AND message_type = 'XXESHWF'
                              AND SYSDATE BETWEEN Trunc(begin_date) AND Trunc(
                                                  end_date);
                   EXCEPTION
                       WHEN OTHERS THEN
                         l_vac_approver := NULL;
                   END;

                   INSERT INTO xxesh_contract_kpi_report
                               (item_type,
                                item_key,
                                actual_user,
                                created_by,
                                creation_date,
                                last_updated_by,
                                last_update_date,
                                last_update_login,
                                role_name,
                                delegated_to)
                   SELECT 'XXESHWF',
                          p_item_key,
                          o_user_name,
                          fnd_global.user_id,
                          SYSDATE,
                          fnd_global.user_id,
                          SYSDATE,
                          fnd_global.login_id,
                          p_department_name,
                          l_vac_approver
                   FROM   dual
                   WHERE  1 = 1
                          AND NOT EXISTS (SELECT 1
                                          FROM   xxesh_contract_kpi_report
                                          WHERE  item_type = 'XXESHWF'
                                                 AND item_key = p_item_key
                                                 AND actual_user = o_user_name
                                                 AND role_name =
                                                     p_department_name
                                         -- AND delegated_to = l_vac_approver
                                         );
               EXCEPTION
                   WHEN OTHERS THEN
                     Write_cust_log(123, 'APPROVER_INFO', 'P_DEPARTMENT_NAME ->'
                                                          || p_department_name
                                                          || ' $  P_ITEM_KEY ->'
                                                          || p_item_key);

                     Write_cust_log(123, 'APPROVER_INFO',
                     'Outtermost Exception backtrace ->'
                     ||
                     dbms_utility.format_error_backtrace
                                                          || '   and stack  -> '
                                                          ||
                     dbms_utility.format_error_stack);
               END;
  END approver_info;
  PROCEDURE Start_wf (pitemkey VARCHAR2)
  IS
    PRAGMA autonomous_transaction;
    vitemtype          VARCHAR2(30) := 'XXESHWF';
    vprocess           VARCHAR2(50) := 'CONTRACT_MAIN_PROCESS';
    vform              VARCHAR2(50);
    vsqlerrm           VARCHAR2(110);
    vdocumment         CLOB;
    vprimarykey        NUMBER;
    vrequestnumber     NUMBER;
    vrequestorname     VARCHAR2(240);
    vrequestormailid   VARCHAR2(240);
    vrequestorusername VARCHAR2(240);
    vrolecount         NUMBER;
    vfiled1            VARCHAR2(300);
    vfiled2            VARCHAR2(300);
    vfiled3            VARCHAR2(300);
    vfiled4            VARCHAR2(300);
    vfiled5            VARCHAR2(300);
    vfiled6            VARCHAR2(300);
    vfiled9            VARCHAR2(300);
    vfiled10           VARCHAR2(300);
    vsubject           VARCHAR2(300);
    vmailbody          VARCHAR2(2000);
  --  vPrimaryKey        NUMBER;
  BEGIN
      wf_engine.Createprocess(itemtype => vitemtype, itemkey => pitemkey,
      process => vprocess);

      wf_engine.Setitemattrtext(vitemtype, pitemkey, 'ITEM_KEY', pitemkey);

      BEGIN
          SELECT head.aggr_ref_num
                 field1,
                 (SELECT name
                  FROM   hr_operating_units
                  WHERE  1 = 1
                         AND organization_id = head.org_id)
                 field2,
                 party_name
                 field3,
                 contract_num
                 field4,
                 CASE
                   WHEN lump_sum_amount IS NOT NULL THEN
                   To_char(lump_sum_amount)
                   WHEN on_demand_unit_price IS NOT NULL THEN
                   To_char(on_demand_unit_price)
                   WHEN other_contract_type IS NOT NULL THEN other_contract_type
                   ELSE NULL
                 END
                 field5,
                 (SELECT Listagg(contract_doc_type, ', ')
                           within GROUP( ORDER BY comm_line_num )
                  FROM   xxesh_contract_doc_list_t t
                  WHERE  t.contract_id = head.contract_id
                         AND t.contract_service_name =
                             head.contract_service_name)
                       field678,
                 'from :'
                 || cont_commence_date
                 || '  to : '
                 || contract_expiry
                 field9,
                 CASE
                   WHEN client_cont_service_list = 'Other' THEN
                   client_cont_oth_comments
                   WHEN client_cont_service_list != 'Other' THEN
                   client_cont_service_list
                   WHEN supply_cont_service_list = 'Other' THEN
                   supply_cont_oth_comments
                   WHEN supply_cont_service_list != 'Other' THEN
                   supply_cont_service_list
                   ELSE NULL
                 END
                 field10
                       ,
                 contract_id
          INTO   vfiled1, vfiled2, vfiled3, vfiled4,
          vfiled5, vfiled6, vfiled9, vfiled10, vprimarykey
          FROM   xxesh_contract_headers_t head
          WHERE  wf_item_key = pitemkey
                 AND wf_item_type = vitemtype;

          wf_engine.Setitemattrtext(vitemtype, pitemkey, 'FIELD1', vfiled1);

          wf_engine.Setitemattrtext(vitemtype, pitemkey, 'FIELD2', vfiled2);

          wf_engine.Setitemattrtext(vitemtype, pitemkey, 'FIELD3', vfiled3);

          wf_engine.Setitemattrtext(vitemtype, pitemkey, 'FIELD4', vfiled4);

          wf_engine.Setitemattrtext(vitemtype, pitemkey, 'FIELD5', vfiled5);

          wf_engine.Setitemattrtext(vitemtype, pitemkey, 'FIELD6', vfiled6);

          wf_engine.Setitemattrtext(vitemtype, pitemkey, 'FIELD9', vfiled9);

          wf_engine.Setitemattrtext(vitemtype, pitemkey, 'FIELD10', vfiled10);

          vsubject := vfiled1
                      ||
          ' Notification of Contract Request/Approval -  between '
                      || vfiled2
                      || ' and '
                      || vfiled3;

          wf_engine.Setitemattrtext(vitemtype, pitemkey, 'NOTIFICATION_SUBJECT',
          vsubject)
          ;

          vmailbody :=
'Hi,     Please note that your review/approval is required on a Contract request No. '
|| vfiled1
|| ', please refer to the following information:             1. Contract Reference '
|| vfiled4
|| '             2. Contract Amount: '
|| vfiled5
|| '             3. Contract Category: '
|| vfiled6
|| '             4. Contract period '
|| vfiled9
|| '             5. Contract Type: '
|| vfiled10
|| ' Thanks, IT Admin';

wf_engine.Setitemattrtext(vitemtype, pitemkey, 'NOTIFICATION_BODY', vmailbody);

--        Wf_Engine.setitemattrdocument (
-- vItemType,
-- pItemKey,
-- 'EXCEL_ATTACHEMENT',
-- 'FND:entity=  &pk1name=  &pk1value='
-- || vPrimaryKey); 
wf_engine.Setitemattrdocument(vitemtype, pitemkey, 'EXCEL_ATTACHMENT',
'FND:entity=XXESH_CONT_REQ_FORM&pk1name=XXESH_CONT_REQ_FORM&pk1value='
||
vprimarykey);
EXCEPTION
    WHEN OTHERS THEN
      Write_cust_log(123, '', 'vPrimaryKey derivation issue -->'
                              || SQLERRM);
END;

-- Wf_Engine.SetItemAttrText (vItemType,
-- pItemKey,
-- '',
-- );
wf_engine.Startprocess(itemtype => vitemtype, itemkey => pitemkey);

COMMIT;
EXCEPTION
  WHEN OTHERS THEN
             Write_cust_log(123, 'start_wf', 'Outtermost Exception backtrace ->'
                                             ||
             dbms_utility.format_error_backtrace
                                             || '   and stack  -> '
                                             ||
             dbms_utility.format_error_stack);

             COMMIT;
END start_wf;
  PROCEDURE Derive_approvers (itemtype IN VARCHAR2,
                              itemkey  IN VARCHAR2,
                              actid    IN NUMBER,
                              funcmode IN VARCHAR2,
                              result   OUT nocopy VARCHAR2)
  AS
    l_contract_id           NUMBER;
    l_contract_num          VARCHAR2(200);
    l_initiator_user_name   VARCHAR2(240);
    l_initiator_email_id    VARCHAR2(240);
    l_initiator_full_name   VARCHAR2(240);
    l_hod_emp_id            NUMBER;
    l_hod_user_name         VARCHAR2(240);
    l_hod_email_id          VARCHAR2(240);
    l_hod_full_name         VARCHAR2(240);
    --------------------------------------
    l_coordinator_user_name VARCHAR2(250);
    l_coordinator_email_id  VARCHAR2(250);
    l_coordinator_full_name VARCHAR2(250);
    l_findept_user_name     VARCHAR2(250);
    l_findept_email_id      VARCHAR2(250);
    l_findept_full_name     VARCHAR2(250);
    --------------------------------------
    l_cm_user_name          VARCHAR2(250);
    l_cm_email_id           VARCHAR2(250);
    l_cm_full_name          VARCHAR2(250);
    l_contra_mana_user_name VARCHAR2(250);
    l_contra_mana_email_id  VARCHAR2(250);
    l_contra_mana_full_name VARCHAR2(250);
    l_vat_manager_user_name VARCHAR2(250);
    l_vat_manager_email_id  VARCHAR2(250);
    l_vat_manager_full_name VARCHAR2(250);
    l_local_dept_user_name  VARCHAR2(250);
    l_local_dept_email_id   VARCHAR2(250);
    l_local_dept_full_name  VARCHAR2(250);
    l_gm_bu_user_name       VARCHAR2(250);
    l_gm_bu_email_id        VARCHAR2(250);
    l_gm_bu_full_name       VARCHAR2(250);
    l_sd_scm_user_name      VARCHAR2(250);
    l_sd_scm_email_id       VARCHAR2(250);
    l_sd_scm_full_name      VARCHAR2(250);
    l_vp_scm_user_name      VARCHAR2(250);
    l_vp_scm_email_id       VARCHAR2(250);
    l_vp_scm_full_name      VARCHAR2(250);
    l_ceo_user_name         VARCHAR2(250);
    l_ceo_email_id          VARCHAR2(250);
    l_ceo_full_name         VARCHAR2(250);
    l_senior_mng_user_name  VARCHAR2(250);
    l_senior_mng_email_id   VARCHAR2(250);
    l_senior_mng_full_name  VARCHAR2(250);
  BEGIN
      Write_cust_log(123, 'derive_approvers', 'itemkey and itemtype ->'
                                              || itemkey
                                              || '  and '
                                              || itemtype);

      BEGIN
          SELECT contract_id,
                 contract_num,
                 hod_emp_id
          INTO   l_contract_id, l_contract_num, l_hod_emp_id
          FROM   xxesh_contract_headers_t
          WHERE  1 = 1
                 AND wf_item_key = itemkey
                 AND wf_item_type = itemtype;
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'derive_approvers',
            'l_contract_id,l_contract_num ->'
            || SQLERRM);
      END;

      BEGIN
          SELECT fndu.user_name,
                 fndu.email_address,
                 papf.full_name
          INTO   l_initiator_user_name, l_initiator_email_id,
                 l_initiator_full_name
          FROM   per_all_people_f papf,
                 fnd_user fndu,
                 xxesh_contract_headers_t head
          WHERE  1 = 1
                 AND fndu.user_id = head.created_by
                 AND SYSDATE BETWEEN papf.effective_start_date AND
                                     papf.effective_end_date
                 AND head.wf_item_key = itemkey
                 AND head.wf_item_type = itemtype
                 AND ROWNUM = 1;

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACTOR_INITIATOR_ROLE', avalue => l_initiator_user_name);
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'derive_approvers',
            'Initiater Derivation error  ->'
            || SQLERRM);
      END;

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#FROM_ROLE', avalue => l_initiator_user_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_FROM', avalue => l_initiator_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_REPLYTO', avalue => l_initiator_email_id);

      BEGIN
          Approver_info('BU/HOD', itemtype, itemkey, l_hod_user_name,
          l_hod_email_id,
          l_hod_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACTOR_HOD_ROLE', avalue => Derive_vacation_rules(itemkey,
                                           l_hod_user_name)
          );
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'derive_approvers',
            'BU/HOD Derivation error  ->'
            || SQLERRM);
      END;

      /*     wf_engine.setitemattrtext(itemtype => itemtype, itemkey => itemkey,
                                   aname => '#FROM_ROLE', avalue => l_hod_full_name);
          wf_engine.setitemattrtext(itemtype => itemtype, itemkey => itemkey,
                                   aname => '#WFM_FROM', avalue => l_hod_full_name);
          wf_engine.setitemattrtext(itemtype => itemtype, itemkey => itemkey,
                                   aname => '#WFM_REPLYTO', avalue => l_hod_email_id);
       */
      -- derive all approvers for easy communication
      BEGIN
          BEGIN
              Approver_info('Contract Coordinators', itemtype, itemkey,
              l_coordinator_user_name, l_coordinator_email_id,
              l_coordinator_full_name
              );

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'CONTRACT_COORDINATOR_ROLE', avalue => Derive_vacation_rules(
                                                     itemkey
                                                     ,
                                                     l_coordinator_user_name));
          EXCEPTION
              WHEN OTHERS THEN
                Write_cust_log(123, 'derive_approvers',
                'Contract Coordinators Derivation error  ->'
                || SQLERRM);
          END;

          BEGIN
              Approver_info('BU/Finance Department', itemtype, itemkey,
              l_findept_user_name,
              l_findept_email_id, l_findept_full_name);

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'FINANCE_DEPT_ROLE', avalue => Derive_vacation_rules(itemkey,
                                             l_findept_user_name));
          EXCEPTION
              WHEN OTHERS THEN
                Write_cust_log(123, 'derive_approvers',
                'BU/Finance Department Derivation error  ->'
                || SQLERRM);
          END;

          ---------------------------------------------------------------------
          BEGIN
              Approver_info('Contract Managers', itemtype, itemkey,
              l_contra_mana_user_name,
              l_contra_mana_email_id, l_contra_mana_full_name);

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'CONTRACT_MANAGER_ROLE', avalue => Derive_vacation_rules(itemkey,
                                                 l_contra_mana_user_name));
          EXCEPTION
              WHEN OTHERS THEN
                Write_cust_log(123, 'derive_approvers',
                'CM Derivation error  ->'
                || SQLERRM);
          END;

          BEGIN
              Approver_info('Compliance Managers', itemtype, itemkey,
              l_cm_user_name,
              l_cm_email_id, l_cm_full_name);

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'CONTRACT_COMPLIANCE_ROLE', avalue => Derive_vacation_rules(
                                                    itemkey,
                                                    l_cm_user_name));
          EXCEPTION
              WHEN OTHERS THEN
                Write_cust_log(123, 'derive_approvers',
                'CM Derivation error  ->'
                || SQLERRM);
          END;

          BEGIN
              Approver_info('VAT Managers', itemtype, itemkey,
              l_vat_manager_user_name
              ,
              l_vat_manager_email_id, l_vat_manager_full_name);

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'VAT_MANAGER_ROLE', avalue => Derive_vacation_rules(itemkey,
                                            l_vat_manager_user_name));
          EXCEPTION
              WHEN OTHERS THEN
                Write_cust_log(123, 'derive_approvers',
                'vat Derivation error  ->'
                || SQLERRM);
          END;

          BEGIN
              Approver_info('Legal Manager', itemtype, itemkey,
              l_local_dept_user_name
              ,
              l_local_dept_email_id, l_local_dept_full_name);

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'LEGAL_DEPT_ROLE', avalue => Derive_vacation_rules(itemkey,
                                           l_local_dept_user_name));
          EXCEPTION
              WHEN OTHERS THEN
                Write_cust_log(123, 'derive_approvers',
                'Legal Manager Derivation error  ->'
                || SQLERRM);
          END;

          ---------------------------------------------
          BEGIN
              Approver_info('SM Contracts', itemtype, itemkey,
              l_senior_mng_user_name,
              l_senior_mng_email_id, l_senior_mng_full_name);

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'CONTRACT_SENIOR_MANAGER_ROLE', avalue => Derive_vacation_rules(
                                                        itemkey,
                                                        l_senior_mng_user_name))
              ;

              Approver_info('GM/BU', itemtype, itemkey, l_gm_bu_user_name,
              l_gm_bu_email_id,
              l_gm_bu_full_name);

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'CONTRACT_GM_BU_ROLE', avalue => Derive_vacation_rules(itemkey,
                                               l_gm_bu_user_name));

              Approver_info('SD/SCM', itemtype, itemkey, l_sd_scm_user_name,
              l_sd_scm_email_id
              , l_sd_scm_full_name);

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'CONTRACT_SD_SCM_ROLE', avalue => Derive_vacation_rules(itemkey,
                                                l_sd_scm_user_name));

              Approver_info('VP/SCM', itemtype, itemkey, l_vp_scm_user_name,
              l_vp_scm_email_id
              , l_vp_scm_full_name);

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'CONTRACT_VP_SCM_ROLE', avalue => Derive_vacation_rules(itemkey,
                                                l_vp_scm_user_name));

              Approver_info('CEO/ESH', itemtype, itemkey, l_ceo_user_name,
              l_ceo_email_id,
              l_ceo_full_name);

              wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey
              ,
              aname =>
              'CONTRACT_CEO_ROLE', avalue => Derive_vacation_rules(itemkey,
                                             l_ceo_user_name));
          EXCEPTION
              WHEN OTHERS THEN
                Write_cust_log(123, 'derive_approvers',
                'Top Manager Derivation error  ->'
                || SQLERRM);
          END;
      END;

      DECLARE
          l_final_noti_role VARCHAR2(100) := 'CONTRACT_APPROVE_'
                                             || itemkey
                                             || '_ROLE';
          l_role_cnt        NUMBER := 0;
          l_roles           VARCHAR2(300);
      BEGIN
          l_role_cnt := 0;

          SELECT Count(1)
          INTO   l_role_cnt
          FROM   apps.wf_roles
          WHERE  name = l_final_noti_role;

          IF l_role_cnt = 0 THEN
            SELECT Listagg(text_value, ' ')
                     within GROUP( ORDER BY text_value ) roles_val
            INTO   l_roles
            FROM   wf_item_attribute_values
            WHERE  1 = 1
                   AND item_type = itemtype
                   AND item_key = itemkey
                   AND name IN ( 'CONTRACT_COORDINATOR_ROLE',
                                 'CONTRACT_MANAGER_ROLE',
                                             'CONTRACTOR_INITIATOR_ROLE' );

            wf_directory.Createadhocrole(l_final_noti_role, l_final_noti_role,
            NULL,
            NULL,
            l_final_noti_role, 'MAILHTML', l_roles, NULL, NULL, 'ACTIVE', NULL);

            wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
            aname
            =>
            'CONTRACT_APPROVAL_ROLE', avalue => l_final_noti_role);
          END IF;
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'derive_approvers',
            'Final FYI notification role derivation error  ->'
            || SQLERRM);
      END;

      UPDATE xxesh_contract_headers_t
      SET    approval_status = 'Sent For Approval',
             last_update_date = SYSDATE,
             last_updated_by = (SELECT user_id
                                FROM   fnd_user
                                WHERE  user_name = wf_engine.context_user
                                       AND ROWNUM = 1)
      WHERE  1 = 1
             AND wf_item_key = itemkey
             AND wf_item_type = itemtype;

      result := 'Success';
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(123, 'derive_approvers',
               'Outtermost Exception backtrace ->'
               || dbms_utility.format_error_backtrace
               || '   and stack  -> '
               || dbms_utility.format_error_stack);

               COMMIT;
  END derive_approvers;
  PROCEDURE Rejected_by_hod (itemtype IN VARCHAR2,
                             itemkey  IN VARCHAR2,
                             actid    IN NUMBER,
                             funcmode IN VARCHAR2,
                             result   OUT nocopy VARCHAR2)
  AS
  BEGIN
      UPDATE xxesh_contract_headers_t
      SET    approval_status = 'Rejected',
             last_update_date = SYSDATE,
             last_updated_by = (SELECT user_id
                                FROM   fnd_user
                                WHERE  user_name = wf_engine.context_user
                                       AND ROWNUM = 1)
      WHERE  1 = 1
             AND wf_item_key = itemkey
             AND wf_item_type = itemtype;

      result := 'Success';
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(123, 'Rejected_by_hod',
               'Outtermost Exception backtrace ->'
               || dbms_utility.format_error_backtrace
               || '   and stack  -> '
               || dbms_utility.format_error_stack);
  END rejected_by_hod;
  PROCEDURE Parallel_approver_derivation (itemtype IN VARCHAR2,
                                          itemkey  IN VARCHAR2,
                                          actid    IN NUMBER,
                                          funcmode IN VARCHAR2,
                                          result   OUT nocopy VARCHAR2)
  AS
    l_coordinator_user_name VARCHAR2(250);
    l_coordinator_email_id  VARCHAR2(250);
    l_coordinator_full_name VARCHAR2(250);
    l_findept_user_name     VARCHAR2(250);
    l_findept_email_id      VARCHAR2(250);
    l_findept_full_name     VARCHAR2(250);
  BEGIN
      BEGIN
          Approver_info('Contract Coordinators', itemtype, itemkey,
          l_coordinator_user_name, l_coordinator_email_id,
          l_coordinator_full_name
          );

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_COORDINATOR_ROLE', avalue => Derive_vacation_rules(itemkey,
                                                 l_coordinator_user_name));
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'derive_approvers',
            'Contract Coordinators Derivation error  ->'
            || SQLERRM);
      END;

      BEGIN
          Approver_info('BU/Finance Department', itemtype, itemkey,
          l_findept_user_name,
          l_findept_email_id, l_findept_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'FINANCE_DEPT_ROLE', avalue => Derive_vacation_rules(itemkey,
                                         l_findept_user_name));
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'derive_approvers',
            'BU/Finance Department Derivation error  ->'
            || SQLERRM);
      END;

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#FROM_ROLE', avalue => l_findept_user_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_FROM', avalue => l_findept_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_REPLYTO', avalue => l_findept_email_id);

      result := 'Success';
  EXCEPTION
    WHEN OTHERS THEN
               result := 'Success';
  END parallel_approver_derivation;
  PROCEDURE Cm_approver_derivation (itemtype IN VARCHAR2,
                                    itemkey  IN VARCHAR2,
                                    actid    IN NUMBER,
                                    funcmode IN VARCHAR2,
                                    result   OUT nocopy VARCHAR2)
  AS
    l_cm_user_name          VARCHAR2(250);
    l_cm_email_id           VARCHAR2(250);
    l_cm_full_name          VARCHAR2(250);
    l_contra_mana_user_name VARCHAR2(250);
    l_contra_mana_email_id  VARCHAR2(250);
    l_contra_mana_full_name VARCHAR2(250);
    l_vat_manager_user_name VARCHAR2(250);
    l_vat_manager_email_id  VARCHAR2(250);
    l_vat_manager_full_name VARCHAR2(250);
    l_local_dept_user_name  VARCHAR2(250);
    l_local_dept_email_id   VARCHAR2(250);
    l_local_dept_full_name  VARCHAR2(250);
    l_gm_bu_user_name       VARCHAR2(250);
    l_gm_bu_email_id        VARCHAR2(250);
    l_gm_bu_full_name       VARCHAR2(250);
    l_sd_scm_user_name      VARCHAR2(250);
    l_sd_scm_email_id       VARCHAR2(250);
    l_sd_scm_full_name      VARCHAR2(250);
    l_vp_scm_user_name      VARCHAR2(250);
    l_vp_scm_email_id       VARCHAR2(250);
    l_vp_scm_full_name      VARCHAR2(250);
    l_ceo_user_name         VARCHAR2(250);
    l_ceo_email_id          VARCHAR2(250);
    l_ceo_full_name         VARCHAR2(250);
    l_senior_mng_user_name  VARCHAR2(250);
    l_senior_mng_email_id   VARCHAR2(250);
    l_senior_mng_full_name  VARCHAR2(250);
  BEGIN
      BEGIN
          Approver_info('Contract Managers', itemtype, itemkey,
          l_contra_mana_user_name,
          l_contra_mana_email_id, l_contra_mana_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_MANAGER_ROLE', avalue => Derive_vacation_rules(itemkey,
                                             l_contra_mana_user_name));
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'cm_approver_derivation',
            'CM Derivation error  ->'
            || SQLERRM);
      END;

      BEGIN
          Approver_info('Compliance Managers', itemtype, itemkey, l_cm_user_name
          ,
          l_cm_email_id, l_cm_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_COMPLIANCE_ROLE', avalue => Derive_vacation_rules(itemkey,
                                                l_cm_user_name));
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'cm_approver_derivation',
            'CM Derivation error  ->'
            || SQLERRM);
      END;

      BEGIN
          Approver_info('VAT Managers', itemtype, itemkey,
          l_vat_manager_user_name
          ,
          l_vat_manager_email_id, l_vat_manager_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'VAT_MANAGER_ROLE', avalue => Derive_vacation_rules(itemkey,
                                        l_vat_manager_user_name));
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'cm_approver_derivation',
            'vat Derivation error  ->'
            || SQLERRM);
      END;

      BEGIN
          Approver_info('Legal Manager', itemtype, itemkey,
          l_local_dept_user_name
          ,
          l_local_dept_email_id, l_local_dept_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'LEGAL_DEPT_ROLE', avalue => Derive_vacation_rules(itemkey,
                                       l_local_dept_user_name));
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'cm_approver_derivation',
            'Legal Manager Derivation error  ->'
            || SQLERRM);
      END;

      ---------------------------------------------
      BEGIN
          Approver_info('SM Contracts', itemtype, itemkey,
          l_senior_mng_user_name,
          l_senior_mng_email_id, l_senior_mng_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_SENIOR_MANAGER_ROLE', avalue => Derive_vacation_rules(
                                                    itemkey,
                                                    l_senior_mng_user_name));

          Approver_info('GM/BU', itemtype, itemkey, l_gm_bu_user_name,
          l_gm_bu_email_id,
          l_gm_bu_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_GM_BU_ROLE', avalue => Derive_vacation_rules(itemkey,
                                           l_gm_bu_user_name));

          Approver_info('SD/SCM', itemtype, itemkey, l_sd_scm_user_name,
          l_sd_scm_email_id
          , l_sd_scm_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_SD_SCM_ROLE', avalue => Derive_vacation_rules(itemkey,
                                            l_sd_scm_user_name));

          Approver_info('VP/SCM', itemtype, itemkey, l_vp_scm_user_name,
          l_vp_scm_email_id
          , l_vp_scm_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_VP_SCM_ROLE', avalue => Derive_vacation_rules(itemkey,
                                            l_vp_scm_user_name));

          Approver_info('CEO/ESH', itemtype, itemkey, l_ceo_user_name,
          l_ceo_email_id,
          l_ceo_full_name);

          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_CEO_ROLE', avalue => Derive_vacation_rules(itemkey,
                                         l_ceo_user_name));
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'cm_approver_derivation',
            'Legal Manager Derivation error  ->'
            || SQLERRM);
      END;

      ----------------------------------------------
      result := 'Success';
  EXCEPTION
    WHEN OTHERS THEN
               result := 'Success';
  END cm_approver_derivation;
  PROCEDURE Vat_manager_app_derivation (itemtype IN VARCHAR2,
                                        itemkey  IN VARCHAR2,
                                        actid    IN NUMBER,
                                        funcmode IN VARCHAR2,
                                        result   OUT nocopy VARCHAR2)
  AS
    l_vat_manager_user_name VARCHAR2(250);
    l_vat_manager_email_id  VARCHAR2(250);
    l_vat_manager_full_name VARCHAR2(250);
  BEGIN
      Approver_info('VAT Managers', itemtype, itemkey, l_vat_manager_user_name,
      l_vat_manager_email_id, l_vat_manager_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      'VAT_MANAGER_ROLE', avalue => Derive_vacation_rules(itemkey,
                                    l_vat_manager_user_name));
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(123, 'VAT_MANAGER_APP_DERIVATION',
               'vat Derivation error  ->'
               || SQLERRM);
  END vat_manager_app_derivation;
  PROCEDURE Additional_app_derivation (itemtype IN VARCHAR2,
                                       itemkey  IN VARCHAR2,
                                       actid    IN NUMBER,
                                       funcmode IN VARCHAR2,
                                       result   OUT nocopy VARCHAR2)
  AS
    l_gm_bu           VARCHAR2(1);
    l_sd_c_scm        VARCHAR2(1);
    l_vp_c_scm        VARCHAR2(1);
    l_ceo_esh         VARCHAR2(1);
    l_attribute1      VARCHAR2(200);
    l_attribute2      VARCHAR2(200);
    l_attribute3      VARCHAR2(200);
    l_appr_gm_bu      VARCHAR2(20);
    l_appr_sd_c_scm   VARCHAR2(20);
    l_appr_vp_c_scm   VARCHAR2(20);
    l_appr_ceo_esh    VARCHAR2(20);
    l_appr_attribute1 VARCHAR2(20);
    l_appr_attribute2 VARCHAR2(20);
    l_appr_attribute3 VARCHAR2(20);
    l_valid           VARCHAR2(1) := 'Y';
    l_notification_id NUMBER := NULL;
  BEGIN
      DELETE xxesh_customer_noti_hierarchy
      WHERE  item_key = itemkey
             AND item_type = itemtype
             AND ( Nvl(gm_bu, 'N') = 'N' )
             AND ( Nvl(sd_c_scm, 'N') = 'N' )
             AND ( Nvl(vp_c_scm, 'N') = 'N' )
             AND ( Nvl(ceo_esh, 'N') = 'N' )
             AND ( Nvl(attribute1, 'N') = 'N' )
             AND ( Nvl(attribute2, 'N') = 'N' )
             AND ( Nvl(attribute3, 'N') = 'N' );

      SELECT Max(attribute15)
      INTO   l_notification_id
      FROM   xxesh_customer_noti_hierarchy
      WHERE  item_key = itemkey
             AND item_type = itemtype;

      BEGIN
          SELECT gm_bu,
                 sd_c_scm,
                 vp_c_scm,
                 ceo_esh,
                 attribute1,
                 attribute2,
                 attribute3,
                 attribute4,
                 attribute5,
                 attribute6,
                 attribute7,
                 attribute8,
                 attribute9,
                 attribute10
          INTO   l_gm_bu, l_sd_c_scm, l_vp_c_scm, l_ceo_esh,
          l_attribute1, l_attribute2, l_attribute3, l_appr_gm_bu,
          l_appr_sd_c_scm, l_appr_vp_c_scm, l_appr_ceo_esh, l_appr_attribute1,
          l_appr_attribute2, l_appr_attribute3
          FROM   xxesh_customer_noti_hierarchy
          WHERE  item_key = itemkey
                 AND item_type = itemtype
                 AND attribute15 = l_notification_id;
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'additional_app_derivation',
            'vat Derivation error  ->'
            || SQLERRM);

            l_valid := 'N';
      END;

      g_contract_gm_bu_role := wf_engine.Getitemattrtext(itemtype => itemtype,
                               itemkey
                               => itemkey, aname =>
                                                        'CONTRACT_GM_BU_ROLE');

      g_contract_sd_scm_role := wf_engine.Getitemattrtext(itemtype => itemtype,
                                itemkey => itemkey, aname =>
                                                          'CONTRACT_SD_SCM_ROLE'
                                );

      g_contract_vp_scm_role := wf_engine.Getitemattrtext(itemtype => itemtype,
                                itemkey => itemkey, aname =>
                                                          'CONTRACT_VP_SCM_ROLE'
                                );

      g_contract_ceo_role := wf_engine.Getitemattrtext(itemtype => itemtype,
                             itemkey
                             => itemkey, aname =>
                                                    'CONTRACT_CEO_ROLE');

      IF l_valid = 'Y' THEN
        IF l_attribute1 IS NOT NULL
           AND l_appr_attribute1 IS NULL THEN
          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_ADD_APP_ROLE', avalue => Derive_vacation_rules(itemkey,
                                             l_attribute1
                                             ));
        ELSIF l_attribute2 IS NOT NULL
              AND l_appr_attribute2 IS NULL THEN
          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_ADD_APP_ROLE', avalue => Derive_vacation_rules(itemkey,
                                             l_attribute2
                                             ));
        ELSIF l_attribute3 IS NOT NULL
              AND l_appr_attribute3 IS NULL THEN
          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_ADD_APP_ROLE', avalue => Derive_vacation_rules(itemkey,
                                             l_attribute3
                                             ));
        ELSIF l_gm_bu = 'Y'
              AND l_appr_gm_bu IS NULL THEN
          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_ADD_APP_ROLE', avalue => Derive_vacation_rules(itemkey,
                                             g_contract_gm_bu_role));
        ELSIF l_sd_c_scm = 'Y'
              AND l_appr_sd_c_scm IS NULL THEN
          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_ADD_APP_ROLE', avalue => Derive_vacation_rules(itemkey,
                                             g_contract_sd_scm_role));
        ELSIF l_vp_c_scm = 'Y'
              AND l_appr_vp_c_scm IS NULL THEN
          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_ADD_APP_ROLE', avalue => Derive_vacation_rules(itemkey,
                                             g_contract_vp_scm_role));
        ELSIF l_ceo_esh = 'Y'
              AND l_appr_ceo_esh IS NULL THEN
          wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey,
          aname =>
          'CONTRACT_ADD_APP_ROLE', avalue => Derive_vacation_rules(itemkey,
                                             g_contract_ceo_role));
        END IF;
      END IF;

      result := 'Success';
  --  resultout := 'COMPLETE:' || 'Y';
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(123, 'additional_app_derivation',
               'Additonal approvers Derivation error  ->'
               || SQLERRM);

               result := 'Success';
  END additional_app_derivation;
  PROCEDURE Next_additional_app_exist (itemtype IN VARCHAR2,
                                       itemkey  IN VARCHAR2,
                                       actid    IN NUMBER,
                                       funcmode IN VARCHAR2,
                                       result   OUT nocopy VARCHAR2)
  AS
    l_current_approver    VARCHAR2(100);
    l_cont_final_app_role VARCHAR2(100);
    l_gm_bu               VARCHAR2(1);
    l_sd_c_scm            VARCHAR2(1);
    l_vp_c_scm            VARCHAR2(1);
    l_ceo_esh             VARCHAR2(1);
    l_attribute1          VARCHAR2(200);
    l_attribute2          VARCHAR2(200);
    l_attribute3          VARCHAR2(200);
    l_appr_gm_bu          VARCHAR2(20);
    l_appr_sd_c_scm       VARCHAR2(20);
    l_appr_vp_c_scm       VARCHAR2(20);
    l_appr_ceo_esh        VARCHAR2(20);
    l_appr_attribute1     VARCHAR2(20);
    l_appr_attribute2     VARCHAR2(20);
    l_appr_attribute3     VARCHAR2(20);
    l_notification_id     NUMBER;
  BEGIN
      l_current_approver := wf_engine.Getitemattrtext(itemtype => itemtype,
                            itemkey => itemkey, aname =>
                                                        'CONTRACT_ADD_APP_ROLE')
      ;

      g_contract_gm_bu_role := wf_engine.Getitemattrtext(itemtype => itemtype,
                               itemkey
                               => itemkey, aname =>
                                                        'CONTRACT_GM_BU_ROLE');

      g_contract_sd_scm_role := wf_engine.Getitemattrtext(itemtype => itemtype,
                                itemkey => itemkey, aname =>
                                                          'CONTRACT_SD_SCM_ROLE'
                                );

      g_contract_vp_scm_role := wf_engine.Getitemattrtext(itemtype => itemtype,
                                itemkey => itemkey, aname =>
                                                          'CONTRACT_VP_SCM_ROLE'
                                );

      g_contract_ceo_role := wf_engine.Getitemattrtext(itemtype => itemtype,
                             itemkey
                             => itemkey, aname =>
                                                    'CONTRACT_CEO_ROLE');

      SELECT Max(attribute15)
      INTO   l_notification_id
      FROM   xxesh_customer_noti_hierarchy
      WHERE  item_key = itemkey
             AND item_type = itemtype;

      BEGIN
          SELECT gm_bu,
                 sd_c_scm,
                 vp_c_scm,
                 ceo_esh,
                 attribute1,
                 attribute2,
                 attribute3,
                 attribute4,
                 attribute5,
                 attribute6,
                 attribute7,
                 attribute8,
                 attribute9,
                 attribute10
          INTO   l_gm_bu, l_sd_c_scm, l_vp_c_scm, l_ceo_esh,
          l_attribute1, l_attribute2, l_attribute3, l_appr_gm_bu,
          l_appr_sd_c_scm, l_appr_vp_c_scm, l_appr_ceo_esh, l_appr_attribute1,
          l_appr_attribute2, l_appr_attribute3
          FROM   xxesh_customer_noti_hierarchy
          WHERE  item_key = itemkey
                 AND item_type = itemtype
                 AND attribute15 = l_notification_id;
      EXCEPTION
          WHEN OTHERS THEN
            Write_cust_log(123, 'additional_app_derivation',
            'vat Derivation error  ->'
            || SQLERRM);
      END;

      /*     
          write_cust_log(123, 'next_additional_app_exist',' l_gm_bu, ->'          ||l_gm_bu);
          write_cust_log(123, 'next_additional_app_exist',' l_sd_c_scm, ->'       ||l_sd_c_scm);
          write_cust_log(123, 'next_additional_app_exist',' l_vp_c_scm, ->'       ||l_vp_c_scm);
          write_cust_log(123, 'next_additional_app_exist',' l_ceo_esh, ->'        ||l_ceo_esh);
          write_cust_log(123, 'next_additional_app_exist',' l_attribute1, ->'     ||l_attribute1);
          write_cust_log(123, 'next_additional_app_exist',' l_attribute2, ->'     ||l_attribute2);
          write_cust_log(123, 'next_additional_app_exist',' l_attribute3, ->'     ||l_attribute3);
          write_cust_log(123, 'next_additional_app_exist',' l_appr_gm_bu, ->'     ||l_appr_gm_bu);
          write_cust_log(123, 'next_additional_app_exist',' l_appr_sd_c_scm, ->'  ||l_appr_sd_c_scm);
          write_cust_log(123, 'next_additional_app_exist',' l_appr_vp_c_scm, ->'  ||l_appr_vp_c_scm);
          write_cust_log(123, 'next_additional_app_exist',' l_appr_ceo_esh, ->'   ||l_appr_ceo_esh);
          write_cust_log(123, 'next_additional_app_exist',' l_appr_attribute1, ->'||l_appr_attribute1);
          write_cust_log(123, 'next_additional_app_exist',' l_appr_attribute2, ->'||l_appr_attribute2);
          write_cust_log(123, 'next_additional_app_exist',' l_appr_attribute3 ->' ||l_appr_attribute3);
       */
      SELECT CASE
               WHEN ceo_esh = 'Y' THEN g_contract_ceo_role
               WHEN vp_c_scm = 'Y' THEN g_contract_vp_scm_role
               WHEN sd_c_scm = 'Y' THEN g_contract_sd_scm_role
               WHEN gm_bu = 'Y' THEN g_contract_gm_bu_role
               WHEN attribute3 IS NOT NULL THEN attribute3
               WHEN attribute2 IS NOT NULL THEN attribute2
               WHEN attribute1 IS NOT NULL THEN attribute1
             END final_app
      INTO   l_cont_final_app_role
      FROM   xxesh_customer_noti_hierarchy
      WHERE  item_type = itemtype
             AND item_key = itemkey
             AND attribute15 = l_notification_id;

      Write_cust_log(123, 'next_additional_app_exist',
      'l_cont_final_app_role and  ->'
      || l_cont_final_app_role
      || '      -      '
      || l_current_approver);

      IF l_attribute1 IS NOT NULL
         AND l_appr_attribute1 IS NULL THEN
        UPDATE xxesh_customer_noti_hierarchy
        SET    attribute8 = 'Y'
        WHERE  item_type = itemtype
               AND item_key = itemkey
               AND attribute15 = l_notification_id;
      ELSIF l_attribute2 IS NOT NULL
            AND l_appr_attribute2 IS NULL THEN
        UPDATE xxesh_customer_noti_hierarchy
        SET    attribute9 = 'Y'
        WHERE  item_type = itemtype
               AND item_key = itemkey
               AND attribute15 = l_notification_id;
      ELSIF l_attribute3 IS NOT NULL
            AND l_appr_attribute3 IS NULL THEN
        UPDATE xxesh_customer_noti_hierarchy
        SET    attribute10 = 'Y'
        WHERE  item_type = itemtype
               AND item_key = itemkey
               AND attribute15 = l_notification_id;
      ELSIF l_gm_bu = 'Y'
            AND l_appr_gm_bu IS NULL THEN
        UPDATE xxesh_customer_noti_hierarchy
        SET    attribute4 = 'Y'
        WHERE  item_type = itemtype
               AND item_key = itemkey
               AND attribute15 = l_notification_id;
      ELSIF l_sd_c_scm = 'Y'
            AND l_appr_sd_c_scm IS NULL THEN
        UPDATE xxesh_customer_noti_hierarchy
        SET    attribute5 = 'Y'
        WHERE  item_type = itemtype
               AND item_key = itemkey
               AND attribute15 = l_notification_id;
      ELSIF l_vp_c_scm = 'Y'
            AND l_appr_vp_c_scm IS NULL THEN
        UPDATE xxesh_customer_noti_hierarchy
        SET    attribute6 = 'Y'
        WHERE  item_type = itemtype
               AND item_key = itemkey
               AND attribute15 = l_notification_id;
      ELSIF l_ceo_esh = 'Y'
            AND l_appr_ceo_esh IS NULL THEN
        UPDATE xxesh_customer_noti_hierarchy
        SET    attribute7 = 'Y'
        WHERE  item_type = itemtype
               AND item_key = itemkey
               AND attribute15 = l_notification_id;
      END IF;

      COMMIT;

      result := 'COMPLETE:'
                || 'Y';

      IF l_current_approver = l_cont_final_app_role THEN
        UPDATE xxesh_contract_headers_t
        SET    approval_status = 'Approved',
               last_update_date = SYSDATE,
               last_updated_by = (SELECT user_id
                                  FROM   fnd_user
                                  WHERE  user_name = wf_engine.context_user
                                         AND ROWNUM = 1)
        WHERE  1 = 1
               AND wf_item_key = itemkey
               AND wf_item_type = itemtype;

        result := 'COMPLETE:'
                  || 'N';
      END IF;
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(123, 'next_additional_app_exist',
               'Next Additonal approvers Derivation error  ->'
               || SQLERRM);

               result := 'COMPLETE:'
                         || 'Y';
  END next_additional_app_exist;
  PROCEDURE Additional_app_rejected (itemtype IN VARCHAR2,
                                     itemkey  IN VARCHAR2,
                                     actid    IN NUMBER,
                                     funcmode IN VARCHAR2,
                                     result   OUT nocopy VARCHAR2)
  AS
  BEGIN
      UPDATE xxesh_customer_noti_hierarchy
      SET    attribute4 = NULL,
             attribute5 = NULL,
             attribute6 = NULL,
             attribute7 = NULL,
             attribute8 = NULL,
             attribute9 = NULL,
             attribute10 = NULL
      WHERE  item_key = itemkey
             AND item_type = itemtype;

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      'CONTRACT_ADD_APP_ROLE', avalue => NULL);

      result := 'Success';
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(123, 'additional_app_rejected',
               'Additonal approvers rejection error  ->'
               || SQLERRM);

               result := 'Success';
  END additional_app_rejected;
  PROCEDURE Hod_mail_content (itemtype IN VARCHAR2,
                              itemkey  IN VARCHAR2,
                              actid    IN NUMBER,
                              funcmode IN VARCHAR2,
                              result   OUT nocopy VARCHAR2)
  AS
    l_hod_user_name VARCHAR2(240);
    l_hod_email_id  VARCHAR2(240);
    l_hod_full_name VARCHAR2(240);
  BEGIN
      Approver_info('BU/HOD', itemtype, itemkey, l_hod_user_name, l_hod_email_id
      ,
      l_hod_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#FROM_ROLE', avalue => l_hod_user_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_FROM', avalue => l_hod_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_REPLYTO', avalue => l_hod_email_id);
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(itemkey, 'hod_mail_content', 'Error is '
                                                           || SQLERRM);
  END hod_mail_content;
  PROCEDURE Contract_co_mail_content (itemtype IN VARCHAR2,
                                      itemkey  IN VARCHAR2,
                                      actid    IN NUMBER,
                                      funcmode IN VARCHAR2,
                                      result   OUT nocopy VARCHAR2)
  AS
    l_coordinator_user_name VARCHAR2(250);
    l_coordinator_email_id  VARCHAR2(250);
    l_coordinator_full_name VARCHAR2(250);
  BEGIN
      Approver_info('Contract Coordinators', itemtype, itemkey,
      l_coordinator_user_name, l_coordinator_email_id, l_coordinator_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#FROM_ROLE', avalue => l_coordinator_user_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_FROM', avalue => l_coordinator_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_REPLYTO', avalue => l_coordinator_email_id);
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(itemkey, 'contract_co_mail_content', 'Error is '
                                                                   || SQLERRM);
  END contract_co_mail_content;
  PROCEDURE Fin_mail_content (itemtype IN VARCHAR2,
                              itemkey  IN VARCHAR2,
                              actid    IN NUMBER,
                              funcmode IN VARCHAR2,
                              result   OUT nocopy VARCHAR2)
  AS
    l_user_name VARCHAR2(250);
    l_email_id  VARCHAR2(250);
    l_full_name VARCHAR2(250);
  BEGIN
      Approver_info('BU/Finance Department', itemtype, itemkey, l_user_name,
      l_email_id, l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#FROM_ROLE', avalue => l_user_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_FROM', avalue => l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_REPLYTO', avalue => l_email_id);
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(itemkey, 'fin_mail_content', 'Error is '
                                                           || SQLERRM);
  END fin_mail_content;
  PROCEDURE Vat_mail_content (itemtype IN VARCHAR2,
                              itemkey  IN VARCHAR2,
                              actid    IN NUMBER,
                              funcmode IN VARCHAR2,
                              result   OUT nocopy VARCHAR2)
  AS
    l_user_name VARCHAR2(250);
    l_email_id  VARCHAR2(250);
    l_full_name VARCHAR2(250);
  BEGIN
      Approver_info('VAT Managers', itemtype, itemkey, l_user_name, l_email_id,
      l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#FROM_ROLE', avalue => l_user_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_FROM', avalue => l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_REPLYTO', avalue => l_email_id);
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(itemkey, 'vat_mail_content', 'Error is '
                                                           || SQLERRM);
  END vat_mail_content;
  PROCEDURE Con_man_mail_content (itemtype IN VARCHAR2,
                                  itemkey  IN VARCHAR2,
                                  actid    IN NUMBER,
                                  funcmode IN VARCHAR2,
                                  result   OUT nocopy VARCHAR2)
  AS
    l_user_name VARCHAR2(250);
    l_email_id  VARCHAR2(250);
    l_full_name VARCHAR2(250);
  BEGIN
      Approver_info('Contract Managers', itemtype, itemkey, l_user_name,
      l_email_id,
      l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#FROM_ROLE', avalue => l_user_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_FROM', avalue => l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_REPLYTO', avalue => l_email_id);
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(itemkey, 'con_man_mail_content', 'Error is '
                                                               || SQLERRM);
  END con_man_mail_content;
  PROCEDURE Complaince_mail_content (itemtype IN VARCHAR2,
                                     itemkey  IN VARCHAR2,
                                     actid    IN NUMBER,
                                     funcmode IN VARCHAR2,
                                     result   OUT nocopy VARCHAR2)
  AS
    l_user_name VARCHAR2(250);
    l_email_id  VARCHAR2(250);
    l_full_name VARCHAR2(250);
  BEGIN
      Approver_info('Compliance Managers', itemtype, itemkey, l_user_name,
      l_email_id,
      l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#FROM_ROLE', avalue => l_user_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_FROM', avalue => l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_REPLYTO', avalue => l_email_id);
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(itemkey, 'complaince_mail_content', 'Error is '
                                                                  || SQLERRM);
  END complaince_mail_content;
  PROCEDURE Legal_dept_mail_content (itemtype IN VARCHAR2,
                                     itemkey  IN VARCHAR2,
                                     actid    IN NUMBER,
                                     funcmode IN VARCHAR2,
                                     result   OUT nocopy VARCHAR2)
  AS
    l_user_name VARCHAR2(250);
    l_email_id  VARCHAR2(250);
    l_full_name VARCHAR2(250);
  BEGIN
      Approver_info('Legal Manager', itemtype, itemkey, l_user_name, l_email_id,
      l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#FROM_ROLE', avalue => l_user_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_FROM', avalue => l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_REPLYTO', avalue => l_email_id);
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(itemkey, 'legal_dept_mail_content', 'Error is '
                                                                  || SQLERRM);
  END legal_dept_mail_content;
  PROCEDURE Con_sm_mail_content (itemtype IN VARCHAR2,
                                 itemkey  IN VARCHAR2,
                                 actid    IN NUMBER,
                                 funcmode IN VARCHAR2,
                                 result   OUT nocopy VARCHAR2)
  AS
    l_user_name VARCHAR2(250);
    l_email_id  VARCHAR2(250);
    l_full_name VARCHAR2(250);
  BEGIN
      Approver_info('SM Contracts', itemtype, itemkey, l_user_name, l_email_id,
      l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#FROM_ROLE', avalue => l_user_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_FROM', avalue => l_full_name);

      wf_engine.Setitemattrtext(itemtype => itemtype, itemkey => itemkey, aname
      =>
      '#WFM_REPLYTO', avalue => l_email_id);
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(itemkey, 'con_sm_mail_content', 'Error is '
                                                              || SQLERRM);
  END con_sm_mail_content;
  PROCEDURE Is_fin_team_approved (itemtype IN VARCHAR2,
                                  itemkey  IN VARCHAR2,
                                  actid    IN NUMBER,
                                  funcmode IN VARCHAR2,
                                  result   OUT nocopy VARCHAR2)
  IS
    l_fin_approved NUMBER;
  BEGIN
      l_fin_approved := 0;

      SELECT Count(1)
      INTO   l_fin_approved
      FROM   wf_notifications a,
             wf_notification_attributes b
      WHERE  1 = 1
             AND a.notification_id = b.notification_id
             AND a.message_type = itemtype
             AND a.item_key = itemkey
             AND a.message_name = 'CONTRACT_FINANCE_MSG'
             AND b.name = 'RESULT'
             AND b.text_value = 'APPROVE';

      IF l_fin_approved = 0 THEN
        result := 'COMPLETE:'
                  || 'N';
      ELSE
        result := 'COMPLETE:'
                  || 'Y';
      END IF;
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(itemkey, 'is_fin_team_approved', 'Error is '
                                                               || SQLERRM);

               result := 'COMPLETE:'
                         || 'N';
  END is_fin_team_approved;
  /*  PROCEDURE is_fin_cc_team_approved (
      itemtype IN VARCHAR2,
      itemkey  IN VARCHAR2,
      actid    IN NUMBER,
      funcmode IN VARCHAR2,
      result   OUT NOCOPY VARCHAR2
    ) IS
      l_fin_approved        NUMBER;
      l_con_coordinator_app NUMBER;
    BEGIN
      SELECT COUNT(1)
      INTO l_fin_approved
      FROM wf_notifications           a,
           wf_notification_attributes b
      WHERE 1 = 1
            AND a.notification_id = b.notification_id
            AND a.message_type = itemtype
            AND a.item_key = itemkey
            AND a.message_name = 'CONTRACT_FINANCE_MSG'
            AND b.name = 'RESULT'
            AND b.text_value = 'APPROVE';
      SELECT COUNT(1)
      INTO l_con_coordinator_app
      FROM wf_notifications           a,
           wf_notification_attributes b
      WHERE 1 = 1
            AND a.notification_id = b.notification_id
            AND a.message_type = itemtype
            AND a.item_key = itemkey
            AND a.message_name = 'CONTRACT_COORDINATOR_MSG'
            AND b.name = 'RESULT'
            AND b.text_value = 'APPROVE';
      IF
        l_fin_approved = 0
        AND l_con_coordinator_app = 0
      THEN
        result := 'COMPLETE:' || 'N';
      ELSIF
        l_fin_approved = 1
        AND l_con_coordinator_app = 0
      THEN
        result := 'COMPLETE:' || 'CC';
      ELSIF
        l_fin_approved = 0
        AND l_con_coordinator_app = 1
      THEN
        result := 'COMPLETE:' || 'FIN';
      ELSIF
        l_fin_approved = 1
        AND l_con_coordinator_app = 1
      THEN
        result := 'COMPLETE:' || 'Y';
      ELSE
        result := 'COMPLETE:' || 'N';
      END IF;
    EXCEPTION
      WHEN OTHERS THEN
        write_cust_log(itemkey, 'is_fin_cc_team_approved',
                      'Error is ' || sqlerrm);
        result := 'COMPLETE:' || 'N';
    END is_fin_cc_team_approved;*/
  FUNCTION Derive_vacation_rules (p_item_key      VARCHAR2,
                                  p_orig_approver VARCHAR2)
  RETURN VARCHAR2
  IS
    l_vac_approver VARCHAR2(50);
  BEGIN
      l_vac_approver := NULL;

      SELECT action_argument
      INTO   l_vac_approver
      FROM   wf_routing_rules
      WHERE  ROLE = p_orig_approver
             AND message_type = 'XXESHWF'
             AND SYSDATE BETWEEN Trunc(begin_date) AND Trunc(end_date);

      INSERT INTO xxesh_contract_vacation_rules
                  (item_type,
                   item_key,
                   created_by,
                   creation_date,
                   actual_user,
                   delegated_to,
                   last_updated_by,
                   last_update_date,
                   last_update_login)
      SELECT 'XXESHWF',
             p_item_key,
             (SELECT user_id
              FROM   fnd_user fu
              WHERE  fu.user_name = l_vac_approver
                     AND ROWNUM = 1),
             SYSDATE,
             p_orig_approver,
             l_vac_approver,
             (SELECT user_id
              FROM   fnd_user fu
              WHERE  fu.user_name = l_vac_approver
                     AND ROWNUM = 1),
             SYSDATE,
             fnd_global.login_id
      FROM   dual
      WHERE  1 = 1
             AND NOT EXISTS (SELECT 1
                             FROM   xxesh_contract_vacation_rules rl1
                             WHERE  1 = 1
                                    AND item_key = p_item_key
                                    AND actual_user = p_orig_approver
                                    AND delegated_to = l_vac_approver);

      RETURN l_vac_approver;
  EXCEPTION
    WHEN no_data_found THEN
               RETURN p_orig_approver; WHEN OTHERS THEN
               Write_cust_log(p_item_key, 'DERIVE_VACATION_RULES', 'Error is '
                                                                   || SQLERRM);

               RETURN p_orig_approver;
  END derive_vacation_rules;
  PROCEDURE Write_cust_log (record_id  IN VARCHAR2,
                            p_log_type IN VARCHAR2,
                            p_log_text IN VARCHAR2)
  AS
    l_seq_id        NUMBER;
    l_overflow_flag VARCHAR2(30);
    l_error_msg     VARCHAR2(4000);
    PRAGMA autonomous_transaction;
  BEGIN
      BEGIN
          SELECT xxesh_contract_cust_log_s.NEXTVAL
          INTO   l_seq_id
          FROM   dual;
      EXCEPTION
          WHEN OTHERS THEN
            l_seq_id := -1;
      END;

      BEGIN
          IF ( Length(p_log_text) > 3990 ) THEN
            l_overflow_flag := '..';
          END IF;

          l_error_msg := Substr(p_log_text, 1, 3990)
                         || l_overflow_flag;

          INSERT INTO xxesh_contract_cust_log
                      (record_id,
                       log_type,
                       seq,
                       log_text,
                       user_id,
                       creation_date)
          VALUES      ( record_id,
                       p_log_type,
                       l_seq_id,
                       l_error_msg,
                       fnd_global.user_id,
                       SYSDATE );

          COMMIT;
      EXCEPTION
          WHEN OTHERS THEN
            l_error_msg := 'Error in Logging'
                           || SQLERRM;

            INSERT INTO xxesh_contract_cust_log
                        (record_id,
                         log_type,
                         seq,
                         log_text,
                         user_id,
                         creation_date)
            VALUES      ( record_id,
                         'CUST_LOG_ERROR',
                         l_seq_id,
                         l_error_msg,
                         fnd_global.user_id,
                         SYSDATE );

            COMMIT;
      END;
  END write_cust_log;
  PROCEDURE Send_mail (p_to        IN VARCHAR2,
                       p_from      IN VARCHAR2,
                       p_subject   IN VARCHAR2,
                       p_text_msg  IN VARCHAR2 DEFAULT NULL,
                       p_html_msg  IN VARCHAR2 DEFAULT NULL,
                       p_smtp_host IN VARCHAR2,
                       p_smtp_port IN NUMBER DEFAULT 25)
  IS
    l_boundary  VARCHAR2(50) := '----=*#abc1234321cba#*=';
    l_mail_conn utl_smtp.connection;
    vtomailids  VARCHAR2(200);
  BEGIN
      fnd_file.Put_line(fnd_file.log, 'p_to       ->'
                                      || p_to);

      fnd_file.Put_line(fnd_file.log, 'p_from     ->'
                                      || p_from);

      fnd_file.Put_line(fnd_file.log, 'p_subject  ->'
                                      || p_subject);

      fnd_file.Put_line(fnd_file.log, 'p_text_msg ->'
                                      || p_text_msg);

      fnd_file.Put_line(fnd_file.log, 'p_html_msg ->'
                                      || p_html_msg);

      fnd_file.Put_line(fnd_file.log, 'p_smtp_host->'
                                      || p_smtp_host);

      l_mail_conn := utl_smtp.Open_connection(p_smtp_host, p_smtp_port);

      utl_smtp.Helo(l_mail_conn, p_smtp_host);

      utl_smtp.Mail(l_mail_conn, p_from);

      --    utl_smtp.rcpt(l_mail_conn, p_to);
      IF p_to NOT LIKE '%,%' THEN
        vtomailids := Rtrim(p_to, ',;');

        utl_smtp.Rcpt(l_mail_conn, vtomailids);
      ELSE
        FOR i IN (SELECT Regexp_substr(p_to, '[^,]+', 1, LEVEL) to_mail_ids
                  FROM   dual
                  CONNECT BY Regexp_substr(p_to, '[^,]+', 1, LEVEL) IS NOT NULL)
        LOOP
            IF Trim(i.to_mail_ids) IS NOT NULL THEN
              utl_smtp.Rcpt(l_mail_conn, Rtrim(i.to_mail_ids, ',;'));

              IF vtomailids IS NULL THEN
                vtomailids := Rtrim(i.to_mail_ids, ',;');
              ELSE
                vtomailids := vtomailids
                              || ','
                              || Rtrim(i.to_mail_ids, ',;');
              END IF;
            END IF;
        END LOOP;
      END IF;

      vtomailids := Replace(Trim(vtomailids), ';', ',');

      utl_smtp.Open_data(l_mail_conn);

      utl_smtp.Write_data(l_mail_conn, 'Date: '
                                       ||
      To_char(SYSDATE, 'DD-MON-YYYY HH24:MI:SS')
                                       || utl_tcp.crlf);

      utl_smtp.Write_data(l_mail_conn, 'To: '
                                       || vtomailids--p_to
                                       || utl_tcp.crlf);

      utl_smtp.Write_data(l_mail_conn, 'From: '
                                       || p_from
                                       || utl_tcp.crlf);

      utl_smtp.Write_data(l_mail_conn, 'Subject: '
                                       || p_subject
                                       || utl_tcp.crlf);

      utl_smtp.Write_data(l_mail_conn, 'Reply-To: '
                                       || p_from
                                       || utl_tcp.crlf);

      utl_smtp.Write_data(l_mail_conn, 'MIME-Version: 1.0'
                                       || utl_tcp.crlf);

      utl_smtp.Write_data(l_mail_conn,
      'Content-Type: multipart/alternative; boundary="'
      || l_boundary
      || '"'
      || utl_tcp.crlf
      || utl_tcp.crlf);

      IF p_text_msg IS NOT NULL THEN
        utl_smtp.Write_data(l_mail_conn, '--'
                                         || l_boundary
                                         || utl_tcp.crlf);

        utl_smtp.Write_data(l_mail_conn,
        'Content-Type: text/plain; charset="iso-8859-1"'
        || utl_tcp.crlf
        || utl_tcp.crlf);

        utl_smtp.Write_data(l_mail_conn, p_text_msg);

        utl_smtp.Write_data(l_mail_conn, utl_tcp.crlf
                                         || utl_tcp.crlf);
      END IF;

      IF p_html_msg IS NOT NULL THEN
        utl_smtp.Write_data(l_mail_conn, '--'
                                         || l_boundary
                                         || utl_tcp.crlf);

        utl_smtp.Write_data(l_mail_conn,
        'Content-Type: text/html; charset="iso-8859-1"'
        || utl_tcp.crlf
        || utl_tcp.crlf);

        utl_smtp.Write_data(l_mail_conn, p_html_msg);

        utl_smtp.Write_data(l_mail_conn, utl_tcp.crlf
                                         || utl_tcp.crlf);
      END IF;

      utl_smtp.Write_data(l_mail_conn, '--'
                                       || l_boundary
                                       || '--'
                                       || utl_tcp.crlf);

      utl_smtp.Close_data(l_mail_conn);

      utl_smtp.Quit(l_mail_conn);
  EXCEPTION
    WHEN OTHERS THEN
               fnd_file.Put_line(fnd_file.log,
               'Error in send_mail outtermost exception ->'
               || dbms_utility.format_error_backtrace
               || '   and place is '
               || dbms_utility.format_error_stack);
  END send_mail;
  PROCEDURE Call_smtp (p_notification_id NUMBER,
                       p_from_mail_id    VARCHAR2,
                       p_to_mail_ids     VARCHAR2,
                       p_subject         VARCHAR2,
                       p_mail_body       VARCHAR2,
                       p_position        NUMBER)
  AS
    l_html         VARCHAR2(32767);
    l_conc_id      NUMBER := fnd_global.conc_request_id;
    l_from_mail_id VARCHAR2(100);
  BEGIN
      l_html := '<!DOCTYPE html> <html> <head> </head> <body> <p>Hi,</br></br>'
                || p_mail_body
                || '</br></p> <p>Thanks,</br>   IT Admin</br></p> </body> </html>';

      l_from_mail_id := Nvl(p_from_mail_id, 'SmtpServer@etisalat.ae');

      Send_mail(p_to => Trim(p_to_mail_ids), p_from => l_from_mail_id,
      p_subject => p_subject
                   || ' and Req Id -'
                   || l_conc_id, p_text_msg => p_mail_body, p_html_msg => NULL
      --l_html
      , p_smtp_host => fnd_profile.Value('XXMMS_MAIL_SMTP_SERVER'));

      IF p_position = 1 THEN
        UPDATE xxesh_timeout_info
        SET    actual_reminder_date1 = SYSDATE
        WHERE  notification_id = p_notification_id;
      ELSIF p_position = 2 THEN
        UPDATE xxesh_timeout_info
        SET    actual_reminder_date2 = SYSDATE
        WHERE  notification_id = p_notification_id;
      ELSIF p_position = 3 THEN
        UPDATE xxesh_timeout_info
        SET    actual_reminder_date3 = SYSDATE
        WHERE  notification_id = p_notification_id;
      ELSIF p_position = 4 THEN
        UPDATE xxesh_timeout_info
        SET    actual_reminder_date4 = SYSDATE
        WHERE  notification_id = p_notification_id;
      ELSE
        NULL;
      END IF;
  EXCEPTION
    WHEN OTHERS THEN
               fnd_file.Put_line(fnd_file.log,
               'Error in call_smtp outtermost exception ->'
               || dbms_utility.format_error_backtrace
               || '   and place is '
               || dbms_utility.format_error_stack);
  END call_smtp;
  PROCEDURE Approve_status (itemtype IN VARCHAR2,
                            itemkey  IN VARCHAR2,
                            actid    IN NUMBER,
                            funcmode IN VARCHAR2,
                            result   OUT nocopy VARCHAR2)
  IS
  BEGIN
      UPDATE xxesh_contract_headers_t
      SET    approval_status = 'APPROVED'
      WHERE  wf_item_key = itemkey
             AND wf_item_type = itemtype;

      result := 'Success';
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(123, 'Approve_Status',
               'Approve_Status error itemkey->'
               || SQLERRM
               || '   ->'
               || itemkey);

               result := 'Success';
  END approve_status;
  PROCEDURE Reject_status (itemtype IN VARCHAR2,
                           itemkey  IN VARCHAR2,
                           actid    IN NUMBER,
                           funcmode IN VARCHAR2,
                           result   OUT nocopy VARCHAR2)
  IS
  BEGIN
      UPDATE xxesh_contract_headers_t
      SET    approval_status = 'REJECTED'
      WHERE  wf_item_key = itemkey
             AND wf_item_type = itemtype;

      result := 'Success';
  EXCEPTION
    WHEN OTHERS THEN
               Write_cust_log(123, 'Reject_Status',
               'Reject_Status error itemkey->'
               || SQLERRM
               || '   ->'
               || itemkey);

               result := 'Success';
  END reject_status;
  FUNCTION Intial_notification (p_contract_num IN VARCHAR2,
                                p_item_key     IN VARCHAR2)
  RETURN VARCHAR2
  AS
    l_hist_item_key VARCHAR2(20);
    l_req_date      VARCHAR2(30);
  BEGIN
      BEGIN
          SELECT wf_item_key
          INTO   l_hist_item_key
          FROM   (SELECT wf_item_key
                  FROM   xxesh_contract_headers_hist_t
                  WHERE  contract_num = p_contract_num
                  ORDER  BY contract_id DESC)
          WHERE  ROWNUM = 1;

          SELECT begin_date
          INTO   l_req_date
          FROM   (SELECT begin_date
                  FROM   wf_notifications
                  WHERE  1 = 1
                         AND message_type = 'XXESHWF'
                         AND item_key = l_hist_item_key
                  ORDER  BY 1)
          WHERE  ROWNUM = 1;

          RETURN To_char(l_req_date);
      EXCEPTION
          WHEN OTHERS THEN
            l_hist_item_key := NULL;
      END;

      BEGIN
          SELECT begin_date
          INTO   l_req_date
          FROM   (SELECT begin_date
                  FROM   wf_notifications
                  WHERE  1 = 1
                         AND message_type = 'XXESHWF'
                         AND item_key = p_item_key
                  ORDER  BY 1)
          WHERE  ROWNUM = 1;

          RETURN To_char(l_req_date);
      EXCEPTION
          WHEN OTHERS THEN
            l_req_date := NULL;
      END;
  EXCEPTION
    WHEN OTHERS THEN
               RETURN SQLERRM;
  END intial_notification;
  FUNCTION Kpi_for_each_dept (p_cont_number IN VARCHAR2,
                              p_role_name   IN VARCHAR2,
                              p_req_id      IN NUMBER)
  RETURN NUMBER
  AS
    l_org_short_name VARCHAR2(20) := NULL;
    l_role_name      VARCHAR2(200) := NULL;
    l_total_time     NUMBER;
    l_t_time         NUMBER;
  BEGIN
      IF p_req_id IS NULL THEN
        /* SELECT SUM(24 * round((wn.end_date - wn.begin_date), 5))
        INTO l_total_time
        FROM xxesh_contract_kpi_report kpi,
             wf_notifications          wn
        WHERE 1 = 1
              AND wn.original_recipient = nvl(kpi.delegated_to, kpi.actual_user)
              AND wn.item_key = kpi.item_key
              AND kpi.role_name = p_role_name
              AND wn.item_key IN --= i.WF_ITEM_KEY
               (
          SELECT wf_item_key
          FROM xxesh_contract_headers_hist_t
          WHERE contract_num = p_cont_number
          UNION ALL
          SELECT wf_item_key
          FROM xxesh_contract_headers_t
          WHERE contract_num = p_cont_number
        ); */
        SELECT SUM(Nvl(Round(( wn.end_date - wn.begin_date ), 5) -
                       (SELECT
                              SUM(CASE
                                  WHEN
                                      To_char(date_val, 'Day') IN(
                                      'Saturday ', 'Sunday   ' )
                                  THEN
                                  1
                                  ELSE
                                  0
                                  END
                              )
                              case_val
                                                                    FROM   (
                       SELECT
                                      SYSDATE - LEVEL + 2000 date_val
                                                                            FROM
                       dual
                                  CONNECT BY LEVEL <= 5000)
                                                                    WHERE  1 = 1
                                                                           AND
                                      date_val >= wn.begin_date
                                                                           AND
                                  date_val <= wn.end_date
                                                                           AND
                       NOT
                                      EXISTS(SELECT 1
                                             FROM   fnd_lookup_values
                                             WHERE  lookup_type =
                                                    'XXESH_CONTRACT_HOLIDAYS'
                                                    AND LANGUAGE = 'US'
                                                    AND description = 'HOLIDAYS'
                                                    AND Trunc(To_date(date_val))
                                                        =
                                                        Trunc
                                                        (
                                                        start_date_active))),
                   Round((
                              wn.end_date - wn.begin_date ), 5))) * 24 days
        --SUM(24 * round((wn.end_date - wn.begin_date), 5))
        INTO   l_total_time
        FROM   xxesh_contract_kpi_report kpi,
               wf_notifications wn
        WHERE  1 = 1
               AND wn.original_recipient =
                   Nvl(kpi.delegated_to, kpi.actual_user)
               AND wn.item_key = kpi.item_key
               AND kpi.role_name = p_role_name--'BU/Finance Department'
               AND wn.item_key IN (SELECT wf_item_key
                                   FROM   xxesh_contract_headers_hist_t
                                   WHERE  contract_num = p_cont_number
                                   UNION ALL
                                   SELECT wf_item_key
                                   FROM   xxesh_contract_headers_t
                                   WHERE  contract_num = p_cont_number);
      ELSE
        SELECT SUM(24 * Round(( wn.end_date - wn.begin_date ), 5))
        INTO   l_total_time
        FROM   wf_notifications wn
        WHERE  1 = 1
               AND wn.original_recipient = (SELECT user_name
                                            FROM   fnd_user
                                            WHERE  1 = 1
                                                   AND user_id = p_req_id
                                                   AND ROWNUM = 1)
               AND wn.item_key IN (SELECT wf_item_key
                                   FROM   xxesh_contract_headers_hist_t
                                   WHERE  contract_num = p_cont_number
                                   UNION ALL
                                   SELECT wf_item_key
                                   FROM   xxesh_contract_headers_t
                                   WHERE  contract_num = p_cont_number);
      END IF;

      RETURN l_total_time;
  EXCEPTION
    WHEN OTHERS THEN
               RETURN 0;
  END kpi_for_each_dept;
END xxesh_contract_approval_pkg; 